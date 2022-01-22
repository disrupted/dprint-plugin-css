use super::configuration::{resolve_config, Configuration};

use anyhow::Result;
use dprint_core::configuration::{ConfigKeyMap, GlobalConfiguration, ResolveConfigurationResult};
use dprint_core::generate_plugin_code;
use dprint_core::plugins::{PluginHandler, PluginInfo};
use std::path::Path;

struct CssPluginHandler {}

impl CssPluginHandler {
    pub const fn new() -> Self {
        CssPluginHandler {}
    }
}

impl PluginHandler<Configuration> for CssPluginHandler {
    fn resolve_config(
        &mut self,
        config: ConfigKeyMap,
        global_config: &GlobalConfiguration,
    ) -> ResolveConfigurationResult<Configuration> {
        resolve_config(config, global_config)
    }

    fn get_plugin_info(&mut self) -> PluginInfo {
        let version = env!("CARGO_PKG_VERSION").to_string();
        PluginInfo {
            name: env!("CARGO_PKG_NAME").to_string(),
            version: version.clone(),
            config_key: "css".to_string(),
            file_extensions: vec!["css".to_string()],
            file_names: vec![],
            help_url: "https://dprint.dev/plugins/css".to_string(),
            config_schema_url: "".to_string(),
        }
    }

    fn get_license_text(&mut self) -> String {
        "License text goes here.".to_string()
    }

    fn format_text(
        &mut self,
        file_path: &Path,
        file_text: &str,
        config: &Configuration,
        _format_with_host: impl FnMut(&Path, String, &ConfigKeyMap) -> Result<String>,
    ) -> Result<String> {
        super::format_text(file_path, file_text, config)
    }
}

// for clearing the configuration in the playground
#[no_mangle]
pub fn reset_config() {
    unsafe {
        RESOLVE_CONFIGURATION_RESULT.get().take();
    }
}

generate_plugin_code!(CssPluginHandler, CssPluginHandler::new());
