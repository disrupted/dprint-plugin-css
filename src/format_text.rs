use super::configuration::Configuration;

use anyhow::Result;
use dprint_core::configuration::resolve_new_line_kind;
use std::path::Path;

pub fn format_text(_file_path: &Path, text: &str, config: &Configuration) -> Result<String> {
    // TODO: format
    let text = text.to_string();

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
            text.replace("\r\n", "\n").replace("\n", "\r\n")
        },
    )
}
