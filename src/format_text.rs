use anyhow::Result;
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::PrintOptions;
use parcel_css::stylesheet::{ParserOptions, PrinterOptions, StyleSheet};
use std::path::Path;

use crate::configuration::Configuration;
use crate::generation::generate;

const PARSER_OPTS: ParserOptions = ParserOptions {
    nesting: true,
    css_modules: false,
    custom_media: false,
    source_index: 0,
};
const PRINTER_OPTS: PrinterOptions = PrinterOptions {
    minify: false,
    source_map: None,
    analyze_dependencies: false,
    targets: None,
    pseudo_classes: None,
};

pub fn format_text(_file_path: &Path, text: &str, config: &Configuration) -> Result<String> {
    let node = parse_node(text)?;

    Ok(dprint_core::formatting::format(
        || generate(node, text, config),
        config_to_print_options(text, config),
    ))
}

fn parse_node(text: &str) -> Result<StyleSheet> {
    let filename = "".to_string();
    match StyleSheet::parse(filename, text, PARSER_OPTS) {
        Ok(v) => Ok(v),
        Err(_) => {
            eprintln!("Error parsing file");
            std::process::exit(1);
        }
    }
}

fn config_to_print_options(text: &str, config: &Configuration) -> PrintOptions {
    PrintOptions {
        indent_width: 1,
        max_width: 80,
        use_tabs: false,
        new_line_text: resolve_new_line_kind(text, config.new_line_kind),
    }
}
