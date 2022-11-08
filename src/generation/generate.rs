// use dprint_core::formatting::ir_helpers::gen_from_raw_string;
use dprint_core::formatting::*;
use raffia::ast::{
    AtRule, Calc, ComplexSelector, ComplexSelectorChild, ComponentValue, Declaration, Delimiter,
    Dimension, Function, InterpolableIdent, InterpolableStr, Number, QualifiedRule, Ratio,
    SimpleBlock, SimpleSelector, Url,
};
use raffia::token::TokenWithSpan;

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
            items.push_str("//");
            items.push_str(comment.as_line().unwrap().content);
        } else if comment.is_block() {
            items.push_str("/*");
            items.push_str(comment.as_block().unwrap().content);
            items.push_str("*/");
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

    items.extend(match node {
        Node::QualifiedRule(node) => gen_qualified_rule_instruction(node),
        Node::Declaration(node) => gen_declaration_instruction(&node),
        Node::AtRule(node) => gen_at_rule_instruction(node),
    });
    items
}

fn gen_qualified_rule_instruction(node: QualifiedRule) -> PrintItems {
    let mut items = PrintItems::new();
    let sel = &node.selector.selectors;
    let complex_selectors: Vec<&ComplexSelectorChild> =
        sel.iter().map(|s| s.children.first().unwrap()).collect();

    for (i, complex_selector) in complex_selectors.iter().enumerate() {
        let simple_selectors = &complex_selector.as_compound_selector().unwrap().children;
        for (_, simple_selector) in simple_selectors.iter().enumerate() {
            items.extend(gen_selector_instruction(simple_selector));
        }
        if i < complex_selectors.len() - 1 {
            items.push_str(",");
            items.push_signal(Signal::NewLine);
        }
    }

    items.extend(parse_simple_block(&node.block));

    items
}

fn gen_at_rule_instruction(node: AtRule) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("@");
    items.push_str(&node.name.name);

    if let Some(block) = &node.block {
        items.extend(parse_simple_block(block));
    }

    items
}

fn gen_complex_selector(complex_selector: &ComplexSelector) -> PrintItems {
    let mut items = PrintItems::new();
    complex_selector
        .children
        .iter()
        .for_each(|c| items.extend(gen_complex_selector_child(c)));
    items
}

fn gen_complex_selector_child(complex_selector_child: &ComplexSelectorChild) -> PrintItems {
    let mut items = PrintItems::new();
    complex_selector_child
        .as_compound_selector()
        .unwrap()
        .children
        .iter()
        .for_each(|c| items.extend(gen_selector_instruction(c)));
    items
}

fn gen_selector_instruction(simple_selector: &SimpleSelector) -> PrintItems {
    let mut items = PrintItems::new();
    if simple_selector.is_type() {
        let typ = simple_selector.as_type().unwrap();
        if typ.is_tag_name() {
            let name = typ
                .as_tag_name()
                .unwrap()
                .name
                .name
                .as_literal()
                .unwrap()
                .name
                .to_string();
            items.push_str(&name);
        } else if typ.is_universal() {
            items.push_str("*");
        }
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
        items.push_str(&class);
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
        items.push_str(&id);
    } else if simple_selector.is_pseudo_element() {
        let name = &simple_selector
            .as_pseudo_element()
            .unwrap()
            .name
            .as_literal()
            .unwrap()
            .name;
        let mut pseudo = "::".to_owned();
        pseudo.push_str(name);
        items.push_str(&pseudo);
    } else if simple_selector.is_pseudo_class() {
        let name = &simple_selector
            .as_pseudo_class()
            .unwrap()
            .name
            .as_literal()
            .unwrap()
            .name;
        items.push_str(":");
        items.push_str(name);
        let arg = &simple_selector.as_pseudo_class().unwrap().arg;
        if arg.is_some() {
            let arg = arg.as_ref().unwrap();
            if arg.is_selector_list() {
                items.push_str("(");
                arg.as_selector_list()
                    .unwrap()
                    .selectors
                    .iter()
                    .for_each(|c| items.extend(gen_complex_selector(c)));
                items.push_str(")");
            }
        }
    } else if simple_selector.is_attribute() {
        items.push_str("[");
        items.extend(parse_interpolable_ident(
            &simple_selector.as_attribute().unwrap().name.name,
        ));
        let attribute = simple_selector.as_attribute().unwrap();
        if let Some(value) = &attribute.value {
            if let Some(matcher) = &attribute.matcher {
                items.push_str(match matcher.kind {
                    raffia::ast::AttributeSelectorMatcherKind::Exact => "=",
                    raffia::ast::AttributeSelectorMatcherKind::MatchWord => "~=",
                    raffia::ast::AttributeSelectorMatcherKind::ExactOrPrefixThenHyphen => "|=",
                    raffia::ast::AttributeSelectorMatcherKind::Prefix => "^=",
                    raffia::ast::AttributeSelectorMatcherKind::Suffix => "$=",
                    raffia::ast::AttributeSelectorMatcherKind::Substring => "*=",
                });
                if value.is_str() {
                    items.extend(parse_interpolable_str(value.as_str().unwrap()));
                } else if value.is_ident() {
                    items.extend(parse_interpolable_ident(value.as_ident().unwrap()));
                }
            }
            if let Some(modifier) = &attribute.modifier {
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_interpolable_ident(&modifier.ident));
            }
        }
        items.push_str("]");
    }
    items
}

fn gen_declaration_instruction(node: &Declaration) -> PrintItems {
    let mut items = PrintItems::new();
    let ident = node.name.as_literal().unwrap().raw;
    items.push_str(ident);
    items.push_str(": ");
    items.extend(parse_component_values(&node.value));

    if node.important.is_some() {
        items.push_str(" !important");
    }
    items
}

fn parse_simple_block(block: &SimpleBlock) -> PrintItems {
    let mut items = PrintItems::new();
    if block.statements.is_empty() {
        items.push_str(" {}");
    } else {
        items.push_str(" {");
        items.push_signal(Signal::NewLine);

        // parse statements inside block
        for statement in &block.statements {
            items.extend(ir_helpers::with_indent({
                let mut items = PrintItems::new();
                if statement.is_declaration() {
                    items.extend(gen_declaration_instruction(
                        statement.as_declaration().unwrap(),
                    ));
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

fn parse_component_values(values: &[ComponentValue]) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, value) in values.iter().enumerate() {
        items.extend(parse_component_value(value));
        if let Some(next_node) = values.get(i + 1) {
            if !next_node.is_delimiter() {
                items.push_signal(Signal::SpaceIfNotTrailing);
            }
        }
    }
    items
}

fn parse_component_value(value: &ComponentValue) -> PrintItems {
    let mut items = PrintItems::new();
    if value.is_delimiter() {
        items.extend(parse_delimiter(value.as_delimiter().unwrap()));
    } else if value.is_interpolable_ident() {
        items.extend(parse_interpolable_ident(
            value.as_interpolable_ident().unwrap(),
        ));
    } else if value.is_interpolable_str() {
        items.extend(parse_interpolable_str(value.as_interpolable_str().unwrap()));
    } else if value.is_dimension() {
        items.extend(parse_dimension(value.as_dimension().unwrap()));
    } else if value.is_number() {
        items.extend(parse_number(value.as_number().unwrap()));
    } else if value.is_percentage() {
        items.push_str(&value.as_percentage().unwrap().value.value.to_string());
        items.push_str("%");
    } else if value.is_hex_color() {
        items.push_str("#");
        items.push_str(&value.as_hex_color().unwrap().value);
    } else if value.is_function() {
        items.extend(parse_function(value.as_function().unwrap()));
    } else if value.is_token_with_span() {
        items.extend(parse_token_with_span(value.as_token_with_span().unwrap()));
    } else if value.is_url() {
        items.extend(parse_url(value.as_url().unwrap()));
    } else if value.is_calc() {
        items.extend(parse_calc(value.as_calc().unwrap()));
    } else if value.is_ratio() {
        items.extend(parse_ratio(value.as_ratio().unwrap()));
    }
    items
}

fn parse_delimiter(delimiter: &Delimiter) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(match delimiter.kind {
        raffia::ast::DelimiterKind::Comma => ",",
        raffia::ast::DelimiterKind::Solidus => "\\",
        raffia::ast::DelimiterKind::Semicolon => ";",
    });
    items
}

fn parse_interpolable_ident(ident: &InterpolableIdent) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(&ident.as_literal().unwrap().name);
    items
}

fn parse_interpolable_str(str: &InterpolableStr) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("\"");
    items.push_str(&str.as_literal().unwrap().value);
    items.push_str("\"");
    items
}

fn parse_dimension(dimension: &Dimension) -> PrintItems {
    let mut items = PrintItems::new();
    if dimension.is_length() {
        let len = dimension.as_length().unwrap();
        items.push_str(&len.value.value.to_string());
        items.push_str(&len.unit.name);
    }
    items
}

fn parse_number(number: &Number) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(&number.value.to_string());
    items
}

fn parse_function(function: &Function) -> PrintItems {
    let mut items = PrintItems::new();
    let name = &function.name.as_literal().unwrap().name;

    items.push_str(name);
    items.push_str("(");
    items.extend(parse_component_values(&function.args));
    items.push_str(")");
    items
}

fn parse_token_with_span(node: &TokenWithSpan) -> PrintItems {
    let mut items = PrintItems::new();
    let token = &node.token;
    if token.is_str() {
        items.push_str(token.as_str().unwrap().raw);
    }
    items
}

fn parse_url(url: &Url) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(&url.name.name);
    items.push_str("(");
    if let Some(value) = &url.value {
        if value.is_raw() {
            items.push_str(&value.as_raw().unwrap().value);
        } else if value.is_str() {
            items.push_str("\"");
            items.push_str(&value.as_str().unwrap().as_literal().unwrap().value);
            items.push_str("\"");
        }
    }
    items.push_str(")");
    items
}

fn parse_calc(calc: &Calc) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_component_value(&calc.left));
    items.push_str(match calc.op.kind {
        raffia::ast::CalcOperatorKind::Plus => " + ",
        raffia::ast::CalcOperatorKind::Minus => " - ",
        raffia::ast::CalcOperatorKind::Multiply => " * ",
        raffia::ast::CalcOperatorKind::Division => " / ",
    });
    items.extend(parse_component_value(&calc.right));
    items
}

fn parse_ratio(ratio: &Ratio) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_number(&ratio.numerator));
    items.push_str("/");
    items.extend(parse_number(&ratio.denominator));
    items
}
