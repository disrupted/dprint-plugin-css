// use dprint_core::formatting::ir_helpers::gen_from_raw_string;
use dprint_core::formatting::*;
use raffia::ast::{
    ComplexSelectorChild, ComponentValue, Declaration, Function, QualifiedRule, SimpleSelector,
};

use super::context::Context;
use super::helpers::*;
use crate::configuration::Configuration;
use crate::format_text::Ast;

pub fn generate<'a>(ast: Ast<'a>, text: &'a str, config: &'a Configuration) -> PrintItems {
    let mut context = Context::new(text, config);
    let mut items = PrintItems::new();
    let top_level_nodes = context.gen_nodes_with_comments(
        0,
        text.len(),
        ast.stylesheet.statements.into_iter().map(|i| i.into()),
    );

    for comment in ast.comments {
        if comment.is_line() {
            items.push_str("// ");
            items.push_str(comment.as_line().unwrap().content);
        } else if comment.is_block() {
            items.push_str("/* ");
            items.push_str(comment.as_block().unwrap().content);
            items.push_str(" */");
        }
        items.push_signal(Signal::NewLine);
    }

    for (i, node) in top_level_nodes.iter().enumerate() {
        items.extend(gen_node(node.clone(), &mut context));
        items.push_signal(Signal::NewLine);
        if let Some(next_node) = top_level_nodes.get(i + 1) {
            //     let text_between = &text[node.span().end..next_node.span().start];
            //     if text_between.chars().filter(|c| *c == '\n').count() > 1 {
            items.push_signal(Signal::NewLine);
            //     }
        }
    }

    items
}

fn gen_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    // context.set_current_node(node.clone());
    items.extend(match node {
        Node::Declaration(node) => gen_declaration_instruction(node, context),
        Node::QualifiedRule(node) => gen_rule_instruction(node, context),
        // Node::Arg(node) => gen_arg_instruction(node, context),
        // Node::Cmd(node) => gen_cmd_instruction(node, context),
        // Node::Copy(node) => gen_copy_instruction(node, context),
        // Node::Entrypoint(node) => gen_entrypoint_instruction(node, context),
        // Node::Env(node) => gen_env_instruction(node, context),
        // Node::EnvVar(node) => gen_env_var(node, context),
        // Node::From(node) => gen_from_instruction(node, context),
        // Node::Label(node) => gen_label_instruction(node, context),
        // Node::LabelLabel(node) => gen_label(node, context),
        // Node::Misc(node) => gen_misc_instruction(node, context),
        // Node::Run(node) => gen_run_instruction(node, context),
        // Node::StringArray(node) => gen_string_array(node, context),
        // Node::String(node) => gen_string(node, context),
        // Node::BreakableString(node) => gen_breakable_string(node, context),
        // Node::CopyFlag(node) => gen_copy_flag(node, context),
        // Node::CommentRc(node) => gen_comment(&node, context),
        // Node::Comment(node) => gen_comment(node, context),
    });
    // context.pop_current_node();
    items
}

fn gen_declaration_instruction<'a>(node: Declaration<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("DECLARATION ");
    // node.statements
    //     .iter()
    //     .map(|i| i.into())
    //     .for_each(|rule| items.extend(gen_node(rule, context)));

    items
}

fn parse_component_value(value: &ComponentValue) -> PrintItems {
    let mut items = PrintItems::new();
    if value.is_delimiter() {
        match value.as_delimiter().unwrap().kind {
            raffia::ast::DelimiterKind::Comma => items.push_str(", "),
            raffia::ast::DelimiterKind::Solidus => items.push_str("\\ "),
            raffia::ast::DelimiterKind::Semicolon => items.push_str("; "),
        };
    } else if value.is_interpolable_ident() {
        items.push_str(
            &value
                .as_interpolable_ident()
                .unwrap()
                .as_literal()
                .unwrap()
                .name,
        );
    } else if value.is_interpolable_str() {
        items.push_str("\"");
        items.push_str(
            &value
                .as_interpolable_str()
                .unwrap()
                .as_literal()
                .unwrap()
                .value,
        );
        items.push_str("\"");
    } else if value.is_dimension() {
        let dimension = value.as_dimension().unwrap();
        if dimension.is_length() {
            let len = dimension.as_length().unwrap();
            items.push_str(&len.value.value.to_string());
            items.push_str(&len.unit.name);
        }
    } else if value.is_number() {
        items.push_str(&value.as_number().unwrap().value.to_string());
    } else if value.is_percentage() {
        let percentage = value.as_percentage().unwrap();
        items.push_str(&percentage.value.value.to_string());
        items.push_str("%");
    } else if value.is_hex_color() {
        items.push_str("#");
        items.push_str(&value.as_hex_color().unwrap().value);
    } else if value.is_function() {
        items.extend(parse_function(value.as_function().unwrap()));
    }
    items
}

fn parse_function(function: &Function) -> PrintItems {
    let mut items = PrintItems::new();
    let name = &function.name.as_literal().unwrap().name;

    items.push_str(name);
    items.push_str("(");
    function
        .args
        .iter()
        .map(parse_component_value)
        .for_each(|p| {
            items.extend(p);
        });
    items.push_str(")");
    items
}

fn gen_rule_instruction<'a>(node: QualifiedRule<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();
    let sel = &node.selector.selectors;
    let complex_selectors: Vec<&ComplexSelectorChild> =
        sel.iter().map(|s| s.children.first().unwrap()).collect();
    let simple_selectors: Vec<&SimpleSelector> = complex_selectors
        .iter()
        .map(|s| s.as_compound_selector().unwrap().children.first().unwrap())
        .collect();

    let mut names: Vec<String> = Vec::new();
    for simple_selector in simple_selectors {
        if simple_selector.is_type() {
            names.push(
                simple_selector
                    .as_type()
                    .unwrap()
                    .as_tag_name()
                    .unwrap()
                    .name
                    .name
                    .as_literal()
                    .unwrap()
                    .name
                    .to_string(),
            );
        } else if simple_selector.is_class() {
            let name = &simple_selector
                .as_class()
                .unwrap()
                .name
                .as_literal()
                .unwrap()
                .name;
            let mut class = ".".to_owned();
            class.push_str(name);
            names.push(class);
        } else if simple_selector.is_id() {
            let name = &simple_selector
                .as_id()
                .unwrap()
                .name
                .as_literal()
                .unwrap()
                .name;
            let mut id = "#".to_owned();
            id.push_str(name);
            names.push(id);
        }
    }

    for (i, name) in names.iter().enumerate() {
        items.push_str(name);
        if i < names.len() - 1 {
            items.push_str(",");
            items.push_signal(Signal::NewLine);
        }
    }

    if node.block.statements.is_empty() {
        items.push_str(" {}");
    } else {
        items.push_str(" {");
        items.push_signal(Signal::NewLine);

        // parse statements inside block
        for statement in &node.block.statements {
            items.extend(ir_helpers::with_indent({
                let mut items = PrintItems::new();
                if statement.is_declaration() {
                    let declaration = statement.as_declaration().unwrap();
                    let ident = declaration.name.as_literal().unwrap().raw;
                    items.push_str(ident);
                    items.push_str(": ");

                    // parse value
                    declaration
                        .value
                        .iter()
                        .for_each(|value| items.extend(parse_component_value(value)));

                    if declaration.important.is_some() {
                        items.push_str(" !important");
                    }
                }
                items.push_str(";");
                items.push_signal(Signal::NewLine);
                items
            }));
        }
        items.push_str("}");
    }

    items
}
