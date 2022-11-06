use std::collections::HashSet;

use super::helpers::*;
use crate::configuration::Configuration;

pub struct Context<'a> {
    pub config: &'a Configuration,
    pub text: &'a str,
    pub handled_comments: HashSet<usize>,
    current_node: Option<Node<'a>>,
    parent_stack: Vec<Node<'a>>,
    pub gen_string_content: bool,
}

impl<'a> Context<'a> {
    pub fn new(text: &'a str, config: &'a Configuration) -> Self {
        Self {
            config,
            text,
            handled_comments: HashSet::new(),
            current_node: None,
            parent_stack: Vec::new(),
            gen_string_content: false,
        }
    }

    pub fn gen_nodes_with_comments(
        &mut self,
        start_pos: usize,
        end_pos: usize,
        nodes: impl Iterator<Item = Node<'a>>,
    ) -> Vec<Node<'a>> {
        let mut result = Vec::new();
        // let mut last_pos = start_pos;
        // for node in nodes {
        //     let text = &self.text[last_pos..node.span().start];
        //     for comment in parse_comments(text, last_pos) {
        //         result.push(Node::CommentRc(Rc::new(comment)));
        //     }
        //     let node_end = node.span().end;
        //     result.push(node);
        //     last_pos = node_end;
        // }
        // let text = &self.text[last_pos..end_pos];
        // for comment in parse_comments(text, last_pos) {
        //     result.push(Node::CommentRc(Rc::new(comment)));
        // }
        result
    }
}
