use super::{Parser, ParserError, ParserResult};
use super::Token;
use super::{not_reserved, escapable};
use super::{Quantifier, RawQuantifier};
use super::combinators::*;
use super::BoxedParser;
// use super::macros;

use super::UnimplementedParser;

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

//detects UNSIGNED integer
pub fn number<'a>() -> impl Parser<'a, usize> {
    one_or_more(any_char.predicate(|c| c.is_ascii_digit())).map(|v| v.into_iter().collect::<String>().parse::<usize>().expect("parser should only generate valid arabic numeral strings"))
}

//FIXME
pub fn expression<'a>() -> impl Parser<'a, ()> {
    pair(sub_expression(), zero_or_more(right( match_literal("|"), sub_expression() ))).map(unimplemented_map)
}

//FIXME
pub fn sub_expression<'a>() -> impl Parser<'a, ()> {
    one_or_more(pqtoken()).map(unimplemented_map)
}

//FIXME
pub fn pqtoken<'a>() -> impl Parser<'a, ()> {
    use quantifiers::quantifier;

    pair(token(), optional(quantifier())).map(unimplemented_map)
}

//FIXME
pub fn token<'a>() -> impl Parser<'a, Token> {
    //macro time
    one_of!(Token, char(), set(), group(), special())
}

pub fn char<'a>() -> impl Parser<'a, Token> {
    either(literal_char(), escaped_char()).map(|c| Token::Char(c))
}

pub fn literal_char<'a>() -> impl Parser<'a, char> {
    any_char.predicate(|c| not_reserved(c))
}

pub fn escaped_char<'a>() -> impl Parser<'a, char> {
    right(match_literal("\\"), any_char.predicate(|c| escapable(c)))
}

//FIX ME
pub fn set<'a>() -> impl Parser<'a, Token> {
    match_literal("").map(|u| Token::Set)
}

pub fn group<'a>() -> impl Parser<'a, Token> {
    either(cgroup(), ncgroup())
}

//FIX ME
pub fn cgroup<'a>() -> impl Parser<'a, Token> {
    left( right(match_literal("("), expression()), match_literal(")") ).map(|e| Token::Group)
}

//FIXME
pub fn ncgroup<'a>() -> impl Parser<'a, Token> {
    left( right(match_literal("(?:"), expression()), match_literal(")") ).map(|e| Token::Group)
}

mod quantifiers {
    //FIXME: CLEAN THIS
    use super::*;
    //should all quantifiers be allowed to be lazy?
    pub fn quantifier<'a>() -> impl Parser<'a, Quantifier> {
        pair(raw::raw_quantifier(), optional(match_literal("?"))).map(|(r, l)| match l {
                                                                            Some(_) => Quantifier::Lazy(r),
                                                                            None => Quantifier::Greedy(r)
                                                                        })
    }
    mod raw {
        //FIXME: CLEAN THIS
        use super::*;

        pub fn raw_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            one_of!(RawQuantifier, range_quantifier(), kleene_star_quantifier(), plus_quantifier(), question_quantifier())
        }
    
        fn range_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            pair(left(right(match_literal("{"), number()), match_literal("..")), left(optional(number()), match_literal("}"))).map(|(l, h)| RawQuantifier::Range(l, h))
        }
    
        fn kleene_star_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("*").map(|u| RawQuantifier::Kleene)
        }
    
        fn plus_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("+").map(|u| RawQuantifier::Kleene)
        }
    
        fn question_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("?").map(|u| RawQuantifier::Kleene)
        }
    }
}

//basically a placeholder for what I'm assuredly forgetting
pub fn special<'a>() -> impl Parser<'a, Token> {
    match_literal("").map(|u| Token::Unimpl)
}

fn unimplemented_map<T>(_in: T) -> () {
    ()
}