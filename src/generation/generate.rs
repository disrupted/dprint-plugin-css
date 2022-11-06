// use dprint_core::formatting::ir_helpers::gen_from_raw_string;
use dprint_core::formatting::*;
use raffia::ast::{Statement, Stylesheet};

use super::context::Context;
use super::helpers::*;
use crate::configuration::Configuration;

pub fn generate<'i>(file: Stylesheet<'i>, text: &'i str, config: &'i Configuration) -> PrintItems {
    let mut context = Context::new(text, file, config);
    let mut items = PrintItems::new();
    let top_level_nodes =
        context.gen_nodes_with_comments(0, text.len(), file.statements.iter().map(|i| i.into()));

    for (i, node) in top_level_nodes.iter().enumerate() {
        items.extend(gen_node(node.clone(), &mut context));
        items.push_signal(Signal::NewLine);
        // if let Some(next_node) = top_level_nodes.get(i + 1) {
        //     let text_between = &text[node.span().end..next_node.span().start];
        //     if text_between.chars().filter(|c| *c == '\n').count() > 1 {
        //         items.push_signal(Signal::NewLine);
        //     }
        // }
    }

    items
}

fn gen_node<'a>(node: Node<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    // context.set_current_node(node.clone());
    items.extend(match node {
        Node::Media(node) => gen_media_instruction(&node, context),
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

fn gen_media_instruction<'a>(node: &'a Statement<'a>, context: &mut Context<'a>) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("ARG ");
    // node.statements
    //     .iter()
    //     .map(|i| i.into())
    //     .for_each(|rule| items.extend(gen_node(rule, context)));

    items
}
