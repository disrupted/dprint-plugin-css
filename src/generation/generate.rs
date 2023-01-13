// use dprint_core::formatting::ir_helpers::gen_from_raw_string;
use dprint_core::formatting::*;
use raffia::ast::{
    AtRule, Calc, Combinator, ComplexSelector, ComplexSelectorChild, ComponentValue,
    CompoundSelector, ContainerCondition, Declaration, Delimiter, Dimension, Function,
    InterpolableIdent, InterpolableStr, KeyframeBlock, MediaConditionKind, MediaFeature,
    MediaFeatureComparisonKind, MediaInParens, NsPrefix, Number, PageSelectorList, QualifiedRule,
    QueryInParens, Ratio, SelectorList, SimpleBlock, SimpleSelector, Url, WqName,
};
use raffia::token::TokenWithSpan;
use raffia::Spanned;

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

    for (i, node) in top_level_nodes.into_iter().enumerate() {
        if i > 0 {
            items.push_signal(Signal::NewLine);
        }
        items.extend(gen_node(node));
        items.push_signal(Signal::NewLine);
    }

    items
}

fn gen_node(node: Node) -> PrintItems {
    let mut items = PrintItems::new();

    items.extend(match node {
        Node::QualifiedRule(node) => gen_qualified_rule_instruction(node),
        Node::Declaration(node) => gen_declaration_instruction(&node),
        Node::AtRule(node) => gen_at_rule_instruction(node),
        Node::KeyframeBlock(node) => gen_keyframe_block(node),
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

    if let Some(prelude) = node.prelude {
        match prelude {
            raffia::ast::AtRulePrelude::Page(_) => (),
            _ => items.push_signal(Signal::SpaceOrNewLine),
        }
        match prelude {
            raffia::ast::AtRulePrelude::Charset(_) => todo!(),
            raffia::ast::AtRulePrelude::ColorProfile(_) => todo!(),
            raffia::ast::AtRulePrelude::Container(container) => {
                if let Some(name) = &container.name {
                    items.extend(parse_interpolable_ident(name));
                }
                items.push_signal(Signal::SpaceOrNewLine);
                items.extend(parse_container_condition(&container.condition));
            }
            raffia::ast::AtRulePrelude::CounterStyle(_) => todo!(),
            raffia::ast::AtRulePrelude::CustomMedia(_) => todo!(),
            raffia::ast::AtRulePrelude::Document(_) => todo!(),
            raffia::ast::AtRulePrelude::FontFeatureValues(_) => todo!(),
            raffia::ast::AtRulePrelude::FontPaletteValues(_) => todo!(),
            raffia::ast::AtRulePrelude::Import(_) => todo!(),
            raffia::ast::AtRulePrelude::Keyframes(keyframes) => match keyframes {
                raffia::ast::KeyframesName::Ident(ident) => {
                    items.extend(parse_interpolable_ident(&ident))
                }
                raffia::ast::KeyframesName::Str(str) => items.extend(parse_interpolable_str(&str)),
            },
            raffia::ast::AtRulePrelude::Layer(layer) => {
                for (i, ident) in layer.idents.iter().enumerate() {
                    items.extend(parse_interpolable_ident(ident));
                    if layer.idents.get(i + 1).is_some() {
                        items.push_str(".");
                    }
                }
            }
            raffia::ast::AtRulePrelude::Media(media) => {
                for (i, query) in media.queries.iter().enumerate() {
                    if i > 0 {
                        items.push_str(",");
                        items.push_signal(Signal::SpaceIfNotTrailing);
                    }
                    match query {
                        raffia::ast::MediaQuery::ConditionOnly(query_condition_only) => {
                            items.extend(parse_media_conditions(&query_condition_only.conditions))
                        }
                        raffia::ast::MediaQuery::WithType(query_with_type) => {
                            if let Some(modifier) = &query_with_type.modifier {
                                items.push_str(&modifier.name);
                                items.push_signal(Signal::SpaceIfNotTrailing);
                            }

                            items.extend(parse_interpolable_ident(&query_with_type.media_type));
                            if let Some(condition) = &query_with_type.condition {
                                items.push_signal(Signal::SpaceIfNotTrailing);
                                items.push_str("and");
                                items.push_signal(Signal::SpaceIfNotTrailing);
                                items.extend(parse_media_conditions(&condition.conditions));
                            }
                        }
                    }
                }
            }
            raffia::ast::AtRulePrelude::Namespace(_) => todo!(),
            raffia::ast::AtRulePrelude::Nest(_) => todo!(),
            raffia::ast::AtRulePrelude::Page(page) => items.extend(parse_page(&page)),
            raffia::ast::AtRulePrelude::PositionFallback(_) => todo!(),
            raffia::ast::AtRulePrelude::Property(_) => todo!(),
            raffia::ast::AtRulePrelude::ScrollTimeline(_) => todo!(),
            raffia::ast::AtRulePrelude::Supports(_) => todo!(),
            raffia::ast::AtRulePrelude::Unknown(unknown) => {
                for token in unknown.tokens {
                    items.extend(parse_token_with_span(&token));
                }
            }
        }
    }

    if let Some(block) = node.block {
        items.extend(parse_simple_block(block));
    }

    items
}

fn parse_page(page: &PageSelectorList) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, selector) in page.selectors.iter().enumerate() {
        if let Some(name) = &selector.name {
            items.push_signal(Signal::SpaceIfNotTrailing);
            items.extend(parse_interpolable_ident(name));
        }
        for pseudo in &selector.pseudo {
            items.push_str(":");
            items.extend(parse_interpolable_ident(&pseudo.name));
        }
        if page.selectors.get(i + 1).is_some() {
            items.push_str(",");
        }
    }
    items
}

fn gen_keyframe_block(node: KeyframeBlock) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, selector) in node.selectors.iter().enumerate() {
        match selector {
            raffia::ast::KeyframeSelector::Ident(ident) => {
                items.extend(parse_interpolable_ident(ident))
            }
            raffia::ast::KeyframeSelector::Percentage(percentage) => {
                items.push_string(percentage.value.value.to_string());
                items.push_str("%");
            }
        }
        if node.selectors.get(i + 1).is_some() {
            items.push_str(",");
            items.push_signal(Signal::SpaceIfNotTrailing);
        }
    }
    items.extend(parse_simple_block(node.block));
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

fn parse_container_condition(condition: &ContainerCondition) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, cond) in condition.conditions.clone().iter().enumerate() {
        match cond {
            raffia::ast::ContainerConditionKind::QueryInParens(query) => {
                items.extend(parse_condition_query_in_parens(query))
            }
            raffia::ast::ContainerConditionKind::And(and) => {
                items.push_str("and");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_condition_query_in_parens(&and.query_in_parens));
            }
            raffia::ast::ContainerConditionKind::Or(or) => {
                items.push_str("or");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_condition_query_in_parens(&or.query_in_parens));
            }
            raffia::ast::ContainerConditionKind::Not(not) => {
                items.push_str("not");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_condition_query_in_parens(&not.query_in_parens));
            }
        }
        if condition.conditions.get(i + 1).is_some() {
            items.push_signal(Signal::SpaceIfNotTrailing);
        }
    }

    items
}

fn parse_condition_query_in_parens(query: &QueryInParens) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("(");
    match query.clone() {
        raffia::ast::QueryInParens::ContainerCondition(nested_condition) => {
            items.extend(parse_container_condition(&nested_condition))
        }
        raffia::ast::QueryInParens::SizeFeature(size_feature) => {
            items.extend(parse_media_feature(&size_feature))
        }
        raffia::ast::QueryInParens::StyleQuery(_) => todo!(),
    }
    items.push_str(")");
    items
}

fn parse_media_conditions(conditions: &[MediaConditionKind]) -> PrintItems {
    let mut items = PrintItems::new();
    for (i, condition) in conditions.iter().enumerate() {
        if i > 0 {
            items.push_signal(Signal::SpaceIfNotTrailing);
        }
        match condition {
            raffia::ast::MediaConditionKind::MediaInParens(media_in_parens) => {
                items.extend(parse_media_in_parens(media_in_parens));
            }
            raffia::ast::MediaConditionKind::And(and) => {
                items.push_str("and");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_media_in_parens(&and.media_in_parens));
            }
            raffia::ast::MediaConditionKind::Or(or) => {
                items.push_str("or");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_media_in_parens(&or.media_in_parens));
            }
            raffia::ast::MediaConditionKind::Not(not) => {
                items.push_str("not");
                items.push_signal(Signal::SpaceIfNotTrailing);
                items.extend(parse_media_in_parens(&not.media_in_parens));
            }
        }
    }
    items
}

fn parse_media_in_parens(media: &MediaInParens) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str("(");
    match media.clone() {
        MediaInParens::MediaCondition(media_condition) => {
            items.extend(parse_media_conditions(&media_condition.conditions))
        }
        MediaInParens::MediaFeature(media_feature) => {
            items.extend(parse_media_feature(&media_feature))
        }
    }
    items.push_str(")");
    items
}

fn parse_media_feature(media_feature: &MediaFeature) -> PrintItems {
    let mut items = PrintItems::new();
    match media_feature {
        raffia::ast::MediaFeature::Plain(plain) => {
            match &plain.name {
                raffia::ast::MediaFeatureName::Ident(ident) => {
                    items.extend(parse_interpolable_ident(ident))
                }
            };
            items.push_str(":");
            items.push_signal(Signal::SpaceIfNotTrailing);
            items.extend(parse_component_value(&plain.value));
        }
        raffia::ast::MediaFeature::Boolean(bool) => match &bool.name {
            raffia::ast::MediaFeatureName::Ident(ident) => {
                items.extend(parse_interpolable_ident(ident))
            }
        },
        raffia::ast::MediaFeature::Range(range) => {
            items.extend(parse_component_value(&range.left));
            items.push_str(parse_media_feature_kind(&range.comparison.kind));
            items.extend(parse_component_value(&range.right));
        }
        raffia::ast::MediaFeature::RangeInterval(range_interval) => {
            items.extend(parse_component_value(&range_interval.left));
            items.push_str(parse_media_feature_kind(
                &range_interval.left_comparison.kind,
            ));
            match &range_interval.name {
                raffia::ast::MediaFeatureName::Ident(ident) => {
                    items.extend(parse_interpolable_ident(ident))
                }
            }
            items.push_str(parse_media_feature_kind(
                &range_interval.right_comparison.kind,
            ));
            items.extend(parse_component_value(&range_interval.right));
        }
    }
    items
}

fn parse_media_feature_kind(media_feature_kind: &MediaFeatureComparisonKind) -> &str {
    match media_feature_kind {
        raffia::ast::MediaFeatureComparisonKind::LessThan => " < ",
        raffia::ast::MediaFeatureComparisonKind::LessThanOrEqual => " <= ",
        raffia::ast::MediaFeatureComparisonKind::GreaterThan => " > ",
        raffia::ast::MediaFeatureComparisonKind::GreaterThanOrEqual => " >= ",
        raffia::ast::MediaFeatureComparisonKind::Equal => " = ",
    }
}

fn parse_combinator(combinator: &Combinator) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(match combinator.kind {
        raffia::ast::CombinatorKind::Descendant => " ",
        raffia::ast::CombinatorKind::NextSibling => " + ",
        raffia::ast::CombinatorKind::Child => " > ",
        raffia::ast::CombinatorKind::LaterSibling => " ~ ",
        raffia::ast::CombinatorKind::Column => " || ",
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
            items.extend(parse_interpolable_ident(&id.name));
        }
        SimpleSelector::Type(typ) => match typ {
            raffia::ast::TypeSelector::TagName(tag_name) => {
                items.extend(parse_wq_name(&tag_name.name));
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
                        raffia::ast::Nth::Integer(int) => items.push_string(int.value.to_string()),
                        raffia::ast::Nth::AnPlusB(an_plus_b) => {
                            if an_plus_b.a.is_negative() {
                                items.push_str("-");
                            }
                            if an_plus_b.a.abs() != 1 {
                                items.push_string(an_plus_b.a.abs().to_string());
                            }
                            items.push_str("n");

                            if an_plus_b.b.is_positive() {
                                items.push_str(" + ");
                                items.push_string(an_plus_b.b.to_string());
                            } else if an_plus_b.b.is_negative() {
                                items.push_str(" - ");
                                items.push_string(an_plus_b.b.abs().to_string());
                            }
                        }
                    },
                    raffia::ast::PseudoClassSelectorArg::Number(number) => {
                        items.push_string(number.value.to_string())
                    }
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
    let statements = block.statements.clone(); // TODO: get rid of clone
    block
        .statements
        .into_iter()
        .enumerate()
        .for_each(|(i, statement)| {
            items.extend(ir_helpers::with_indent({
                let mut items = PrintItems::new();
                if i > 0 {
                    if let Some(prev_statement) = &statements.get(i - 1) {
                        if !statement.is_declaration() || !prev_statement.is_declaration() {
                            items.push_signal(Signal::NewLine);
                        }
                    }
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
        let next_node = values.get(i + 1);
        match next_node {
            None | Some(ComponentValue::Delimiter(_)) => (),
            Some(ComponentValue::TokenWithSpan(next_token)) => {
                if next_token.span.start > value.span().end {
                    items.push_signal(Signal::SpaceOrNewLine)
                }
            }
            _ => items.push_signal(Signal::SpaceOrNewLine),
        }
    }
    items
}

fn parse_component_value(value: &ComponentValue) -> PrintItems {
    let mut items = PrintItems::new();
    match value {
        ComponentValue::BracketBlock(bracket_block) => {
            items.push_str("[");
            items.extend(parse_component_values(&bracket_block.value));
            items.push_str("]");
        }
        ComponentValue::Calc(node) => items.extend(parse_calc(node)),
        ComponentValue::Delimiter(node) => items.extend(parse_delimiter(node)),
        ComponentValue::Dimension(node) => items.extend(parse_dimension(node)),
        ComponentValue::Function(node) => items.extend(parse_function(node)),
        ComponentValue::HexColor(node) => {
            items.push_str("#");
            items.push_str(&node.value);
        }
        ComponentValue::IdSelector(id_selector) => {
            items.push_str("#");
            items.extend(parse_interpolable_ident(&id_selector.name));
        }
        ComponentValue::InterpolableIdent(node) => items.extend(parse_interpolable_ident(node)),
        ComponentValue::InterpolableStr(node) => items.extend(parse_interpolable_str(node)),
        ComponentValue::LayerName(layer_name) => {
            for ident in &layer_name.idents {
                items.extend(parse_interpolable_ident(ident));
            }
        }
        ComponentValue::Number(node) => items.extend(parse_number(node)),
        ComponentValue::Percentage(node) => {
            items.push_string(node.value.value.to_string());
            items.push_str("%");
        }
        ComponentValue::Ratio(node) => items.extend(parse_ratio(node)),
        ComponentValue::TokenWithSpan(node) => items.extend(parse_token_with_span(node)),
        ComponentValue::UnicodeRange(_) => todo!(),
        ComponentValue::Url(node) => items.extend(parse_url(node)),
        ComponentValue::SassBinaryExpression(_) => todo!(),
        ComponentValue::SassMap(_) => todo!(),
        ComponentValue::SassNamespacedExpression(_) => todo!(),
        ComponentValue::SassNestingDeclaration(_) => todo!(),
        ComponentValue::SassParenthesizedExpression(_) => todo!(),
        ComponentValue::SassParentSelector(_) => todo!(),
        ComponentValue::SassUnaryExpression(_) => todo!(),
        ComponentValue::SassVariable(_) => todo!(),
        ComponentValue::LessVariable(_) => todo!(),
        ComponentValue::LessVariableVariable(_) => todo!(),
    };
    items
}

fn parse_delimiter(delimiter: &Delimiter) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(match delimiter.kind {
        raffia::ast::DelimiterKind::Comma => ",",
        raffia::ast::DelimiterKind::Solidus => " /",
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
            items.push_string(len.value.value.to_string());
            items.push_str(&len.unit.name);
        }
        Dimension::Angle(angle) => {
            items.push_string(angle.value.value.to_string());
            items.push_str(&angle.unit.name);
        }
        Dimension::Duration(_) => todo!(),
        Dimension::Frequency(_) => todo!(),
        Dimension::Resolution(resolution) => {
            items.push_string(resolution.value.value.to_string());
            items.push_str(&resolution.unit.name);
        }
        Dimension::Flex(_) => todo!(),
        Dimension::Unknown(unknown) => {
            items.push_string(unknown.value.value.to_string());
            items.push_str(&unknown.unit.name);
        }
    }
    items
}

fn parse_number(number: &Number) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_string(number.value.to_string());
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
    match &node.token {
        raffia::token::Token::Eof(_) => items.push_str("<eof>"),
        raffia::token::Token::Ampersand(_) => items.push_str("&"),
        raffia::token::Token::Asterisk(_) => items.push_str("*"),
        raffia::token::Token::AsteriskEqual(_) => items.push_str("*="),
        raffia::token::Token::At(_) => items.push_str("@"),
        raffia::token::Token::AtKeyword(_) => items.push_str("<at-keyword>"),
        raffia::token::Token::AtLBraceVar(_) => items.push_str("@{"),
        raffia::token::Token::BadStr(_) => items.push_str("<bad string>"),
        raffia::token::Token::Bar(_) => items.push_str("|"),
        raffia::token::Token::BarBar(_) => items.push_str("||"),
        raffia::token::Token::BarEqual(_) => items.push_str("|="),
        raffia::token::Token::CaretEqual(_) => items.push_str("^="),
        raffia::token::Token::Cdc(_) => items.push_str("<CDC>"),
        raffia::token::Token::Cdo(_) => items.push_str("<CDO>"),
        raffia::token::Token::Colon(_) => items.push_str(":"),
        raffia::token::Token::ColonColon(_) => items.push_str("::"),
        raffia::token::Token::Comma(_) => items.push_str(","),
        raffia::token::Token::Dedent(_) => items.push_str("<dedent>"),
        raffia::token::Token::Dimension(dimension) => {
            items.push_str(dimension.value.raw);
            items.push_str(dimension.unit.raw);
        }
        raffia::token::Token::DollarEqual(_) => items.push_str("$="),
        raffia::token::Token::DollarVar(var) => {
            items.push_str("$");
            items.push_str(var.ident.raw);
        }
        raffia::token::Token::Dot(_) => items.push_str("."),
        raffia::token::Token::DotDotDot(_) => items.push_str("..."),
        raffia::token::Token::Equal(_) => items.push_str("="),
        raffia::token::Token::EqualEqual(_) => items.push_str("=="),
        raffia::token::Token::Exclamation(_) => items.push_str("!"),
        raffia::token::Token::ExclamationEqual(_) => items.push_str("!="),
        raffia::token::Token::GreaterThan(_) => items.push_str(">"),
        raffia::token::Token::GreaterThanEqual(_) => items.push_str(">="),
        raffia::token::Token::Hash(_) => items.push_str("#"),
        raffia::token::Token::HashLBrace(_) => items.push_str("#{"),
        raffia::token::Token::Ident(ident) => items.push_str(ident.raw),
        raffia::token::Token::Indent(_) => items.push_str("<indent>"),
        raffia::token::Token::LBrace(_) => items.push_str("{"),
        raffia::token::Token::LBracket(_) => items.push_str("["),
        raffia::token::Token::LessThan(_) => items.push_str("<"),
        raffia::token::Token::LessThanEqual(_) => items.push_str("<="),
        raffia::token::Token::Linebreak(_) => items.push_str("\n"),
        raffia::token::Token::LParen(_) => items.push_str("("),
        raffia::token::Token::Minus(_) => items.push_str("-"),
        raffia::token::Token::Number(number) => items.push_str(number.raw),
        raffia::token::Token::NumberSign(_) => items.push_str("#"),
        raffia::token::Token::Percent(_) => items.push_str("%"),
        raffia::token::Token::Percentage(percentage) => {
            items.push_str(percentage.value.raw);
            items.push_str("%");
        }
        raffia::token::Token::Plus(_) => items.push_str("+"),
        raffia::token::Token::PlusUnderscore(_) => items.push_str("+_"),
        raffia::token::Token::Question(_) => items.push_str("?"),
        raffia::token::Token::RBrace(_) => items.push_str("}"),
        raffia::token::Token::RBracket(_) => items.push_str("]"),
        raffia::token::Token::RParen(_) => items.push_str(")"),
        raffia::token::Token::Semicolon(_) => items.push_str(";"),
        raffia::token::Token::Solidus(_) => items.push_str("\\"),
        raffia::token::Token::Str(str) => items.push_str(str.raw),
        raffia::token::Token::StrTemplate(str_template) => items.push_str(str_template.raw),
        raffia::token::Token::Tilde(_) => items.push_str("~"),
        raffia::token::Token::TildeEqual(_) => items.push_str("~="),
        raffia::token::Token::UrlRaw(url_raw) => items.push_str(url_raw.raw),
        raffia::token::Token::UrlTemplate(url_template) => items.push_str(url_template.raw),
    };
    items
}

fn parse_url(url: &Url) -> PrintItems {
    let mut items = PrintItems::new();
    items.push_str(&url.name.name);
    items.push_str("(");
    if let Some(value) = &url.value {
        match value {
            raffia::ast::UrlValue::Raw(raw) => items.push_str(&raw.value),
            raffia::ast::UrlValue::Str(str) => items.extend(parse_interpolable_str(str)),
            raffia::ast::UrlValue::SassInterpolated(_) => todo!(),
        }
    }
    items.push_str(")");
    items
}

fn parse_calc(calc: &Calc) -> PrintItems {
    let mut items = PrintItems::new();
    let is_left_calc = calc.left.is_calc();
    let is_right_calc = calc.right.is_calc();
    let should_surround_left = is_left_calc
        && !is_right_calc
        && !matches!(
            calc.left.as_calc().unwrap().op.kind,
            raffia::ast::CalcOperatorKind::Multiply | raffia::ast::CalcOperatorKind::Division
        );
    if should_surround_left {
        items.push_str("(");
    }
    items.extend(parse_component_value(&calc.left));
    if should_surround_left {
        items.push_str(")");
    }
    items.push_str(match calc.op.kind {
        raffia::ast::CalcOperatorKind::Plus => " + ",
        raffia::ast::CalcOperatorKind::Minus => " - ",
        raffia::ast::CalcOperatorKind::Multiply => " * ",
        raffia::ast::CalcOperatorKind::Division => " / ",
    });
    let should_surround_right = !is_left_calc
        && is_right_calc
        && !matches!(
            calc.right.as_calc().unwrap().op.kind,
            raffia::ast::CalcOperatorKind::Multiply | raffia::ast::CalcOperatorKind::Division
        );
    if should_surround_right {
        items.push_str("(");
    }
    items.extend(parse_component_value(&calc.right));
    if should_surround_right {
        items.push_str(")");
    }
    items
}

fn parse_ratio(ratio: &Ratio) -> PrintItems {
    let mut items = PrintItems::new();
    items.extend(parse_number(&ratio.numerator));
    items.push_str("/");
    items.extend(parse_number(&ratio.denominator));
    items
}
