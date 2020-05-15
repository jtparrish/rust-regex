use super::{Parser, ParserError, ParserResult};
use super::ParseElement;
use super::{not_reserved, escapable};
use super::combinators::*;

pub fn match_literal<'a>(expected: &str) -> impl Parser<'a, ()> {
    let owned_expect = expected.to_owned();

    move |input: &'a str|
        match input.get(0..owned_expect.len()) {
            Some(next) if next == owned_expect => Ok(( (), &input[owned_expect.len()..] )),
            _ => Err(ParserError::Error(input.to_owned())),
        }
}

pub fn any_char(input: &str) -> ParserResult<char> {
    input.chars().next().map( |c| (c, &input[c.len_utf8()..]) ).ok_or(ParserError::Error(input.to_owned()))
}

pub fn regex_literal<'a>() -> impl Parser<'a, ParseElement> {
    either(any_char.predicate(|c| not_reserved(c)), escaped_character()).map(|c| ParseElement::MatchChar(c))
}

pub fn escaped_character<'a>() -> impl Parser<'a, char> {
    right(match_literal("\\"), any_char).predicate(|c| escapable(c))
}

//TODO
pub fn quantifier(input: &str) -> ParserResult<char> {
    Ok(('c', input))
}

pub fn quantified_character<'a>() -> impl Parser<'a, ParseElement> {
    quantified(
        either(any_char.predicate(|c| not_reserved(c)).map(|c| ParseElement::Literal(c.to_string())),
            escaped_character().map(|c| ParseElement::Literal(c.to_string()))
        )
    )
}