use dprint_core::configuration::{
    resolve_global_config, ConfigKeyMap, ConfigKeyValue, GlobalConfiguration, NewLineKind,
};
use std::collections::HashMap;

use super::*;

/// Formatting configuration builder.
///
/// # Example
///
/// ```
/// use dprint_plugin_css::configuration::*;
///
/// let config = ConfigurationBuilder::new()
///     .build();
/// ```
#[derive(Default)]
pub struct ConfigurationBuilder {
    pub(super) config: ConfigKeyMap,
    global_config: Option<GlobalConfiguration>,
}

impl ConfigurationBuilder {
    /// Constructs a new configuration builder.
    pub fn new() -> ConfigurationBuilder {
        ConfigurationBuilder {
            config: HashMap::new(),
            global_config: None,
        }
    }

    /// Gets the final configuration that can be used to format a file.
    pub fn build(&self) -> Configuration {
        if let Some(global_config) = &self.global_config {
            resolve_config(self.config.clone(), global_config).config
        } else {
            let global_config = resolve_global_config(HashMap::new(), &Default::default()).config;
            resolve_config(self.config.clone(), &global_config).config
        }
    }

    /// Set the global configuration.
    pub fn global_config(&mut self, global_config: GlobalConfiguration) -> &mut Self {
        self.global_config = Some(global_config);
        self
    }

    /// Whether to use tabs (true) or spaces (false).
    ///
    /// Default: `false`
    pub fn use_tabs(&mut self, value: bool) -> &mut Self {
        self.insert("useTabs", value.into())
    }

    /// The number of columns for an indent.
    ///
    /// Default: `4`
    pub fn indent_width(&mut self, value: u8) -> &mut Self {
        self.insert("indentWidth", (value as i32).into())
    }

    /// The kind of newline to use.
    /// Default: `NewLineKind::LineFeed`
    pub fn new_line_kind(&mut self, value: NewLineKind) -> &mut Self {
        self.insert("newLineKind", value.to_string().into())
    }

    #[cfg(test)]
    pub(super) fn get_inner_config(&self) -> ConfigKeyMap {
        self.config.clone()
    }

    fn insert(&mut self, name: &str, value: ConfigKeyValue) -> &mut Self {
        self.config.insert(String::from(name), value);
        self
    }
}

#[cfg(test)]
mod tests {
    use dprint_core::configuration::{resolve_global_config, NewLineKind};
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn check_all_values_set() {
        let mut config = ConfigurationBuilder::default();
        config
            .new_line_kind(NewLineKind::CarriageReturnLineFeed)
            .use_tabs(true)
            .indent_width(2);

        let inner_config = config.get_inner_config();
        assert_eq!(inner_config.len(), 3);
        let diagnostics = resolve_config(
            inner_config,
            &resolve_global_config(HashMap::new(), &Default::default()).config,
        )
        .diagnostics;
        assert_eq!(diagnostics.len(), 0);
    }

    #[test]
    fn handle_global_config() {
        let mut global_config = HashMap::new();
        global_config.insert(String::from("newLineKind"), "crlf".into());
        global_config.insert(String::from("useTabs"), true.into());
        let global_config = resolve_global_config(global_config, &Default::default()).config;
        let mut config_builder = ConfigurationBuilder::new();
        let config = config_builder.global_config(global_config).build();
        assert!(config.new_line_kind == NewLineKind::CarriageReturnLineFeed);
        assert!(config.use_tabs);
    }

    #[test]
    fn use_defaults_when_global_not_set() {
        let global_config = resolve_global_config(HashMap::new(), &Default::default()).config;
        let mut config_builder = ConfigurationBuilder::new();
        let config = config_builder.global_config(global_config).build();
        assert!(config.new_line_kind == NewLineKind::LineFeed);
        assert_eq!(config.indent_width, 4);
    }
}
