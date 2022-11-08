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
        match comment {
            raffia::token::Comment::Block(block) => {
                items.push_str("/*");
                items.push_str(block.content);
                items.push_str("*/");
            }
            raffia::token::Comment::Line(line) => {
                items.push_str("//");
                items.push_str(line.content);
            }
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
    match simple_selector {
        SimpleSelector::Class(class) => {
            items.push_str(".");
            items.push_str(&class.name.as_literal().unwrap().name);
        }
        SimpleSelector::Id(id) => {
            items.push_str("#");
            items.push_str(&id.name.as_literal().unwrap().name);
        }
        SimpleSelector::Type(typ) => match typ {
            raffia::ast::TypeSelector::TagName(tag_name) => {
                items.push_str(&tag_name.name.name.as_literal().unwrap().name);
            }
            raffia::ast::TypeSelector::Universal(_) => items.push_str("*"),
        },
        SimpleSelector::Attribute(attribute) => {
            items.push_str("[");
            items.extend(parse_interpolable_ident(&attribute.name.name));
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
                    match value {
                        raffia::ast::AttributeSelectorValue::Ident(ident) => {
                            items.extend(parse_interpolable_ident(ident))
                        }
                        raffia::ast::AttributeSelectorValue::Str(str) => {
                            items.extend(parse_interpolable_str(str))
                        }
                    }
                }
                if let Some(modifier) = &attribute.modifier {
                    items.push_signal(Signal::SpaceIfNotTrailing);
                    items.extend(parse_interpolable_ident(&modifier.ident));
                }
            }
            items.push_str("]");
        }
        SimpleSelector::PseudoClass(pseudo_class) => {
            items.push_str(":");
            items.push_str(&pseudo_class.name.as_literal().unwrap().name);
            if let Some(arg) = &pseudo_class.arg {
                match &arg {
                    raffia::ast::PseudoClassSelectorArg::CompoundSelector(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::CompoundSelectorList(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::Ident(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::LanguageRangeList(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::Nth(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::Number(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::RelativeSelectorList(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::SelectorList(selector_list) => {
                        items.push_str("(");
                        arg.as_selector_list()
                            .unwrap()
                            .selectors
                            .iter()
                            .for_each(|c| items.extend(gen_complex_selector(c)));
                        items.push_str(")");
                    }
                    raffia::ast::PseudoClassSelectorArg::TokenSeq(_) => todo!(),
                }
            }
        }
        SimpleSelector::PseudoElement(pseudo_element) => {
            items.push_str("::");
            items.push_str(&pseudo_element.name.as_literal().unwrap().name);
        }
        SimpleSelector::Nesting(nesting) => todo!(),
        SimpleSelector::SassPlaceholder(_) => todo!(),
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
                // TODO: gen_node
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
    match value {
        ComponentValue::BracketBlock(_) => todo!(),
        ComponentValue::Calc(node) => items.extend(parse_calc(node)),
        ComponentValue::Delimiter(node) => items.extend(parse_delimiter(node)),
        ComponentValue::Dimension(node) => items.extend(parse_dimension(node)),
        ComponentValue::Function(node) => items.extend(parse_function(node)),
        ComponentValue::HexColor(node) => {
            items.push_str("#");
            items.push_str(&node.value);
        }
        ComponentValue::IdSelector(_) => todo!(),
        ComponentValue::InterpolableIdent(node) => items.extend(parse_interpolable_ident(node)),
        ComponentValue::InterpolableStr(node) => items.extend(parse_interpolable_str(node)),
        ComponentValue::LayerName(_) => todo!(),
        ComponentValue::LessVariable(_) => todo!(),
        ComponentValue::LessVariableVariable(_) => todo!(),
        ComponentValue::Number(node) => items.extend(parse_number(node)),
        ComponentValue::Percentage(node) => {
            items.push_str(&node.value.value.to_string());
            items.push_str("%");
        }
        ComponentValue::Ratio(node) => items.extend(parse_ratio(node)),
        ComponentValue::SassBinaryExpression(_) => todo!(),
        ComponentValue::SassMap(_) => todo!(),
        ComponentValue::SassNamespacedExpression(_) => todo!(),
        ComponentValue::SassNestingDeclaration(_) => todo!(),
        ComponentValue::SassParenthesizedExpression(_) => todo!(),
        ComponentValue::SassParentSelector(_) => todo!(),
        ComponentValue::SassUnaryExpression(_) => todo!(),
        ComponentValue::SassVariable(_) => todo!(),
        ComponentValue::TokenWithSpan(node) => items.extend(parse_token_with_span(node)),
        ComponentValue::UnicodeRange(_) => todo!(),
        ComponentValue::Url(node) => items.extend(parse_url(node)),
    };
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
    items.push_str(match &node.token {
        raffia::token::Token::Eof(_) => todo!(),
        raffia::token::Token::Ampersand(_) => todo!(),
        raffia::token::Token::Asterisk(_) => todo!(),
        raffia::token::Token::AsteriskEqual(_) => todo!(),
        raffia::token::Token::At(_) => todo!(),
        raffia::token::Token::AtKeyword(_) => todo!(),
        raffia::token::Token::AtLBraceVar(_) => todo!(),
        raffia::token::Token::BadStr(_) => todo!(),
        raffia::token::Token::Bar(_) => todo!(),
        raffia::token::Token::BarBar(_) => todo!(),
        raffia::token::Token::BarEqual(_) => todo!(),
        raffia::token::Token::CaretEqual(_) => todo!(),
        raffia::token::Token::Cdc(_) => todo!(),
        raffia::token::Token::Cdo(_) => todo!(),
        raffia::token::Token::Colon(_) => todo!(),
        raffia::token::Token::ColonColon(_) => todo!(),
        raffia::token::Token::Comma(_) => todo!(),
        raffia::token::Token::Dedent(_) => todo!(),
        raffia::token::Token::Dimension(_) => todo!(),
        raffia::token::Token::DollarEqual(_) => todo!(),
        raffia::token::Token::DollarVar(_) => todo!(),
        raffia::token::Token::Dot(_) => todo!(),
        raffia::token::Token::DotDotDot(_) => todo!(),
        raffia::token::Token::Equal(_) => todo!(),
        raffia::token::Token::EqualEqual(_) => todo!(),
        raffia::token::Token::Exclamation(_) => todo!(),
        raffia::token::Token::ExclamationEqual(_) => todo!(),
        raffia::token::Token::GreaterThan(_) => todo!(),
        raffia::token::Token::GreaterThanEqual(_) => todo!(),
        raffia::token::Token::Hash(_) => todo!(),
        raffia::token::Token::HashLBrace(_) => todo!(),
        raffia::token::Token::Ident(_) => todo!(),
        raffia::token::Token::Indent(_) => todo!(),
        raffia::token::Token::LBrace(_) => todo!(),
        raffia::token::Token::LBracket(_) => todo!(),
        raffia::token::Token::LessThan(_) => todo!(),
        raffia::token::Token::LessThanEqual(_) => todo!(),
        raffia::token::Token::Linebreak(_) => todo!(),
        raffia::token::Token::LParen(_) => todo!(),
        raffia::token::Token::Minus(_) => todo!(),
        raffia::token::Token::Number(_) => todo!(),
        raffia::token::Token::NumberSign(_) => todo!(),
        raffia::token::Token::Percent(_) => todo!(),
        raffia::token::Token::Percentage(_) => todo!(),
        raffia::token::Token::Plus(_) => todo!(),
        raffia::token::Token::PlusUnderscore(_) => todo!(),
        raffia::token::Token::Question(_) => todo!(),
        raffia::token::Token::RBrace(_) => todo!(),
        raffia::token::Token::RBracket(_) => todo!(),
        raffia::token::Token::RParen(_) => todo!(),
        raffia::token::Token::Semicolon(_) => todo!(),
        raffia::token::Token::Solidus(_) => todo!(),
        raffia::token::Token::Str(str) => str.raw,
        raffia::token::Token::StrTemplate(_) => todo!(),
        raffia::token::Token::Tilde(_) => todo!(),
        raffia::token::Token::TildeEqual(_) => todo!(),
        raffia::token::Token::UrlRaw(_) => todo!(),
        raffia::token::Token::UrlTemplate(_) => todo!(),
    });
    items
}

fn parse_url(url: &Url) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(&url.name.name);
    items.push_str("(");
    if let Some(value) = &url.value {
        match value {
            raffia::ast::UrlValue::Raw(raw) => items.push_str(&raw.value),
            raffia::ast::UrlValue::SassInterpolated(_) => todo!(),
            raffia::ast::UrlValue::Str(str) => {
                items.push_str("\"");
                items.push_str(&str.as_literal().unwrap().value);
                items.push_str("\"");
            }
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
