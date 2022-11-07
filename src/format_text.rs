use anyhow::{anyhow, Result};
use dprint_core::configuration::resolve_new_line_kind;
use dprint_core::formatting::PrintOptions;
use raffia::ast::Stylesheet;
use raffia::token::Comment;
use raffia::ParserBuilder;
use std::path::Path;

use crate::configuration::Configuration;
use crate::generation::generate;

pub struct Ast<'a> {
    pub stylesheet: Stylesheet<'a>,
    pub comments: Vec<Comment<'a>>,
}

pub fn format_text(_file_path: &Path, text: &str, config: &Configuration) -> Result<String> {
    let ast = parse_ast(text)?;

    Ok(dprint_core::formatting::format(
        || generate(ast, text, config),
        config_to_print_options(text, config),
    ))
}

fn parse_ast(text: &str) -> Result<Ast> {
    let mut comments = vec![];
    let mut parser = ParserBuilder::new(text).comments(&mut comments).build();
    let stylesheet = parser
        .parse::<Stylesheet>()
        .map_err(|err| anyhow!("parser error {:#?}", err))?;
    Ok(Ast {
        stylesheet,
        comments,
    })
}

fn config_to_print_options(text: &str, config: &Configuration) -> PrintOptions {
    PrintOptions {
        indent_width: config.indent_width,
        max_width: config.line_width,
        use_tabs: config.use_tabs,
        new_line_text: resolve_new_line_kind(text, config.new_line_kind),
    }
}
