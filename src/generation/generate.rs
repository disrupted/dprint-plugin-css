// use dprint_core::formatting::ir_helpers::gen_from_raw_string;
use dprint_core::formatting::*;
use raffia::ast::{
    AtRule, Calc, Combinator, ComplexSelector, ComplexSelectorChild, ComponentValue,
    CompoundSelector, Declaration, Delimiter, Dimension, Function, InterpolableIdent,
    InterpolableStr, NsPrefix, Number, QualifiedRule, Ratio, SelectorList, SimpleBlock,
    SimpleSelector, Url, WqName,
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
        items.extend(gen_node(node.clone()));
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

fn gen_node(node: Node) -> PrintItems {
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
    items.extend(gen_selector_list(&node.selector));
    items.extend(parse_simple_block(node.block));
    items
}

fn gen_at_rule_instruction(node: AtRule) -> PrintItems {
    let mut items = PrintItems::new();

    items.push_str("@");
    items.push_str(&node.name.name);

    if let Some(block) = node.block {
        items.extend(parse_simple_block(block));
    }

    items
}

fn gen_selector_list(selector: &SelectorList) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, complex_selector) in selector.selectors.iter().enumerate() {
        if i > 0 {
            items.push_str(",");
            items.push_signal(Signal::NewLine);
        }
        items.extend(gen_complex_selector(complex_selector));
    }
    items
}

fn gen_complex_selector(complex_selector: &ComplexSelector) -> PrintItems {
    let mut items = PrintItems::new();
    complex_selector
        .children
        .iter()
        .for_each(|complex_selector_child| {
            items.extend(gen_complex_selector_child(complex_selector_child))
        });
    items
}

fn gen_complex_selector_child(complex_selector_child: &ComplexSelectorChild) -> PrintItems {
    let mut items = PrintItems::new();
    match complex_selector_child {
        ComplexSelectorChild::CompoundSelector(compound_selector) => {
            items.extend(gen_compound_selector(compound_selector))
        }
        ComplexSelectorChild::Combinator(combinator) => items.extend(parse_combinator(combinator)),
    }
    items
}

fn gen_compound_selector(compound_selector: &CompoundSelector) -> PrintItems {
    let mut items = PrintItems::new();
    compound_selector
        .children
        .iter()
        .for_each(|simple_selector| items.extend(gen_selector_instruction(simple_selector)));
    items
}

fn parse_combinator(combinator: &Combinator) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(match combinator.kind {
        raffia::ast::CombinatorKind::Descendant => " ",
        raffia::ast::CombinatorKind::NextSibling => " + ",
        raffia::ast::CombinatorKind::Child => " > ",
        raffia::ast::CombinatorKind::LaterSibling => " ~ ",
        raffia::ast::CombinatorKind::Column => "||",
    });
    items
}

fn parse_wq_name(wq_name: &WqName) -> PrintItems {
    let mut items = PrintItems::new();

    if let Some(ns_prefix) = &wq_name.prefix {
        items.extend(parse_ns_prefix(ns_prefix));
    }
    items.extend(parse_interpolable_ident(&wq_name.name));
    items
}

fn parse_ns_prefix(ns_prefix: &NsPrefix) -> PrintItems {
    let mut items = PrintItems::new();
    if let Some(kind) = &ns_prefix.kind {
        match kind {
            raffia::ast::NsPrefixKind::Ident(ident) => {
                items.extend(parse_interpolable_ident(ident))
            }
            raffia::ast::NsPrefixKind::Universal(_) => items.push_str("*"),
        }
        items.push_str("|");
    }
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
            raffia::ast::TypeSelector::Universal(universal) => {
                if let Some(ns_prefix) = &universal.prefix {
                    items.extend(parse_ns_prefix(ns_prefix))
                }
                items.push_str("*");
            }
        },
        SimpleSelector::Attribute(attribute) => {
            items.push_str("[");
            items.extend(parse_wq_name(&attribute.name));
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
                items.push_str("(");
                match &arg {
                    raffia::ast::PseudoClassSelectorArg::CompoundSelector(compound_selector) => {
                        items.extend(gen_compound_selector(compound_selector))
                    }
                    raffia::ast::PseudoClassSelectorArg::CompoundSelectorList(
                        compound_selector_list,
                    ) => compound_selector_list
                        .selectors
                        .iter()
                        .enumerate()
                        .for_each(|(i, compound_selector)| {
                            items.extend(gen_compound_selector(compound_selector));
                            if i < compound_selector_list.selectors.len() - 1 {
                                items.push_str(",");
                                items.push_signal(Signal::SpaceOrNewLine);
                            }
                        }),

                    raffia::ast::PseudoClassSelectorArg::Ident(ident) => {
                        items.extend(parse_interpolable_ident(ident))
                    }
                    raffia::ast::PseudoClassSelectorArg::LanguageRangeList(language_range_list) => {
                        language_range_list.ranges.iter().enumerate().for_each(
                            |(i, language_range)| {
                                match language_range {
                                    raffia::ast::LanguageRange::Str(str) => {
                                        items.extend(parse_interpolable_str(str))
                                    }
                                    raffia::ast::LanguageRange::Ident(ident) => {
                                        items.extend(parse_interpolable_ident(ident))
                                    }
                                }
                                if i < language_range_list.ranges.len() - 1 {
                                    items.push_str(",");
                                    items.push_signal(Signal::SpaceOrNewLine);
                                }
                            },
                        )
                    }
                    raffia::ast::PseudoClassSelectorArg::Nth(nth) => match nth {
                        raffia::ast::Nth::Odd(odd) => items.push_str(&odd.name),
                        raffia::ast::Nth::Even(even) => items.push_str(&even.name),
                        raffia::ast::Nth::Integer(int) => items.push_str(&int.value.to_string()),
                        raffia::ast::Nth::AnPlusB(an_plus_b) => {
                            if an_plus_b.a.is_negative() {
                                items.push_str("-");
                            }
                            if an_plus_b.a.abs() != 1 {
                                items.push_str(&an_plus_b.a.abs().to_string());
                            }
                            items.push_str("n");

                            if an_plus_b.b.is_positive() {
                                items.push_str(" + ");
                                items.push_str(&an_plus_b.b.to_string());
                            } else if an_plus_b.b.is_negative() {
                                items.push_str(" - ");
                                items.push_str(&an_plus_b.b.abs().to_string());
                            }
                        }
                    },
                    raffia::ast::PseudoClassSelectorArg::Number(_) => todo!(),
                    raffia::ast::PseudoClassSelectorArg::RelativeSelectorList(
                        relative_selector_list,
                    ) => {
                        for (i, selector) in relative_selector_list.selectors.iter().enumerate() {
                            items.extend(gen_complex_selector(&selector.complex_selector));
                            if let Some(combinator) = &selector.combinator {
                                items.extend(parse_combinator(combinator));
                            }
                            if i < relative_selector_list.selectors.len() - 1 {
                                items.push_str(",");
                                items.push_signal(Signal::SpaceOrNewLine);
                            }
                        }
                    }
                    raffia::ast::PseudoClassSelectorArg::SelectorList(selector_list) => {
                        selector_list.selectors.iter().enumerate().for_each(
                            |(i, complex_selector)| {
                                items.extend(gen_complex_selector(complex_selector));
                                if i < selector_list.selectors.len() - 1 {
                                    items.push_str(",");
                                    items.push_signal(Signal::SpaceOrNewLine);
                                }
                            },
                        );
                    }
                    raffia::ast::PseudoClassSelectorArg::TokenSeq(token_seq) => {
                        token_seq.tokens.iter().enumerate().for_each(|(i, token)| {
                            items.extend(parse_token_with_span(token));
                            if let Some(next_token) = token_seq.tokens.get(i + 1) {
                                if next_token.span.start > token.span.end {
                                    items.push_signal(Signal::SpaceOrNewLine);
                                }
                            }
                        })
                    }
                }
                items.push_str(")");
            }
        }
        SimpleSelector::PseudoElement(pseudo_element) => {
            items.push_str("::");
            items.push_str(&pseudo_element.name.as_literal().unwrap().name);
        }
        SimpleSelector::Nesting(_) => items.push_str("&"),
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

    items.push_str(";");
    items
}

fn parse_simple_block(block: SimpleBlock) -> PrintItems {
    let mut items = PrintItems::new();
    if block.statements.is_empty() {
        items.push_str(" {}");
        return items;
    }
    items.push_str(" {");
    items.push_signal(Signal::NewLine);

    // parse statements inside block
    block
        .statements
        .into_iter()
        .enumerate()
        .for_each(|(i, statement)| {
            items.extend(ir_helpers::with_indent({
                let mut items = PrintItems::new();
                if i > 0 && !statement.is_declaration() {
                    items.push_signal(Signal::NewLine);
                }
                items.extend(gen_node(statement.into()));
                items.push_signal(Signal::NewLine);
                items
            }));
        });
    items.push_str("}");

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
    items.push_str(ident.as_literal().unwrap().raw);
    items
}

fn parse_interpolable_str(str: &InterpolableStr) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("\"");
    items.push_str(&str.as_literal().unwrap().value.replace('\"', "\\\""));
    items.push_str("\"");
    items
}

fn parse_dimension(dimension: &Dimension) -> PrintItems {
    let mut items = PrintItems::new();
    match dimension {
        Dimension::Length(len) => {
            items.push_str(&len.value.value.to_string());
            items.push_str(&len.unit.name);
        }
        Dimension::Angle(_) => todo!(),
        Dimension::Duration(_) => todo!(),
        Dimension::Frequency(_) => todo!(),
        Dimension::Resolution(_) => todo!(),
        Dimension::Flex(_) => todo!(),
        Dimension::Unknown(_) => todo!(),
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
        raffia::token::Token::Ampersand(_) => "&",
        raffia::token::Token::Asterisk(_) => "*",
        raffia::token::Token::AsteriskEqual(_) => "*=",
        raffia::token::Token::At(_) => "@",
        raffia::token::Token::AtKeyword(_) => todo!(),
        raffia::token::Token::AtLBraceVar(_) => todo!(),
        raffia::token::Token::BadStr(_) => todo!(),
        raffia::token::Token::Bar(_) => "|",
        raffia::token::Token::BarBar(_) => "||",
        raffia::token::Token::BarEqual(_) => "|=",
        raffia::token::Token::CaretEqual(_) => "^=",
        raffia::token::Token::Cdc(_) => todo!(),
        raffia::token::Token::Cdo(_) => todo!(),
        raffia::token::Token::Colon(_) => ":",
        raffia::token::Token::ColonColon(_) => "::",
        raffia::token::Token::Comma(_) => ",",
        raffia::token::Token::Dedent(_) => todo!(),
        raffia::token::Token::Dimension(_) => todo!(),
        raffia::token::Token::DollarEqual(_) => "$=",
        raffia::token::Token::DollarVar(_) => todo!(),
        raffia::token::Token::Dot(_) => ".",
        raffia::token::Token::DotDotDot(_) => "...",
        raffia::token::Token::Equal(_) => "=",
        raffia::token::Token::EqualEqual(_) => "==",
        raffia::token::Token::Exclamation(_) => "!",
        raffia::token::Token::ExclamationEqual(_) => "!=",
        raffia::token::Token::GreaterThan(_) => ">",
        raffia::token::Token::GreaterThanEqual(_) => ">=",
        raffia::token::Token::Hash(_) => "#",
        raffia::token::Token::HashLBrace(_) => "#{",
        raffia::token::Token::Ident(ident) => ident.raw,
        raffia::token::Token::Indent(_) => todo!(),
        raffia::token::Token::LBrace(_) => "{",
        raffia::token::Token::LBracket(_) => "[",
        raffia::token::Token::LessThan(_) => "<",
        raffia::token::Token::LessThanEqual(_) => "<=",
        raffia::token::Token::Linebreak(_) => "\n",
        raffia::token::Token::LParen(_) => "(",
        raffia::token::Token::Minus(_) => "-",
        raffia::token::Token::Number(number) => number.raw,
        raffia::token::Token::NumberSign(_) => "#",
        raffia::token::Token::Percent(_) => "%",
        raffia::token::Token::Percentage(_) => "%",
        raffia::token::Token::Plus(_) => "+",
        raffia::token::Token::PlusUnderscore(_) => "+_",
        raffia::token::Token::Question(_) => "?",
        raffia::token::Token::RBrace(_) => "}",
        raffia::token::Token::RBracket(_) => "]",
        raffia::token::Token::RParen(_) => ")",
        raffia::token::Token::Semicolon(_) => ";",
        raffia::token::Token::Solidus(_) => "\\",
        raffia::token::Token::Str(str) => str.raw,
        raffia::token::Token::StrTemplate(str_template) => str_template.raw,
        raffia::token::Token::Tilde(_) => "~",
        raffia::token::Token::TildeEqual(_) => "~=",
        raffia::token::Token::UrlRaw(url_raw) => url_raw.raw,
        raffia::token::Token::UrlTemplate(url_template) => url_template.raw,
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
            raffia::ast::UrlValue::Str(str) => items.extend(parse_interpolable_str(str)),
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
