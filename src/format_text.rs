use super::configuration::Configuration;

use anyhow::Result;
use dprint_core::configuration::resolve_new_line_kind;
use lightningcss::stylesheet::{ParserOptions, PrinterOptions, StyleSheet};
use std::path::Path;

const PRINTER_OPTS: PrinterOptions = PrinterOptions {
    minify: false,
    source_map: None,
    targets: None,
    analyze_dependencies: false,
    pseudo_classes: None,
};

pub fn format_text(_file_path: &Path, text: &str, config: &Configuration) -> Result<String> {
    let parser_opts = ParserOptions {
        nesting: true,
        custom_media: false,
        css_modules: None,
        source_index: 0,
        filename: String::new(),
        error_recovery: false,
        warnings: None,
    };
    let stylesheet = match StyleSheet::parse(text, parser_opts) {
        Ok(v) => v,
        Err(_) => {
            eprintln!("Error parsing file");
            std::process::exit(1);
        }
    };
    let css = stylesheet.to_css(PRINTER_OPTS).unwrap();
    let text = css.code;

    // ensure ends with newline
    let text = if !text.ends_with('\n') {
        let mut text = text;
        text.push('\n');
        text
    } else {
        text
    };

    // newline
    Ok(
        if resolve_new_line_kind(&text, config.new_line_kind) == "\n" {
            text.replace("\r\n", "\n")
        } else {
            // lazy
            text.replace("\r\n", "\n").replace('\n', "\r\n")
        },
    )
}
