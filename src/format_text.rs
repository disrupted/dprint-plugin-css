use anyhow::{anyhow, Result};
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::PrintOptions;
use raffia::{ast::Stylesheet, Parser, Syntax};
use std::path::Path;

use crate::configuration::Configuration;
use crate::generation::generate;

pub fn format_text(_file_path: &Path, text: &str, config: &Configuration) -> Result<String> {
    let node = parse_node(text)?;

    Ok(dprint_core::formatting::format(
        || generate(node, text, config),
        config_to_print_options(text, config),
    ))
}

fn parse_node(text: &str) -> Result<Stylesheet> {
    let mut parser = Parser::new(text, Syntax::Css);
    parser
        .parse::<Stylesheet>()
        .map_err(|err| anyhow!("raffia error")) // TODO
}

fn config_to_print_options(text: &str, config: &Configuration) -> PrintOptions {
    PrintOptions {
        indent_width: 1,
        max_width: 80,
        use_tabs: false,
        new_line_text: resolve_new_line_kind(text, config.new_line_kind),
    }
}
