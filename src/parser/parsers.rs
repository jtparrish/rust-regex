use super::{Parser, ParserError, ParserResult};
use super::Token;
use super::{not_reserved, escapable, map_literal_charset, map_unicode_charset};
use super::{Quantifier, RawQuantifier};
use super::combinators::*;
use super::{Expression, SubExpression};
use super::QuantifiedToken;
use super::BoxedParser;
// use super::macros;
use super::{CharClass, RawCharClass, IndirectMatch, CharSet, UnicodeCategory, Group, Anchor, map_anchor};

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

pub mod char {
    //FIXME CLEANME
    use super::*;
    pub fn char<'a>() -> impl Parser<'a, char> {
        either(literal_char(), escaped_char())
    }
    
    pub fn literal_char<'a>() -> impl Parser<'a, char> {
        any_char.predicate(|c| not_reserved(c))
    }
    
    pub fn escaped_char<'a>() -> impl Parser<'a, char> {
        right(match_literal("\\"), any_char.predicate(|c| escapable(c)))
    }
}

//TODO: consider putting first sub_expression at the back
pub fn expression<'a>() -> impl Parser<'a, Expression> {
    pair(sub_expression(), zero_or_more(right( match_literal("|"), sub_expression() )))
        .map(|(f, mut v)| {
            v.insert(0, f);
            Expression::new(v)
        })
}

//FIXME
pub fn sub_expression<'a>() -> impl Parser<'a, SubExpression> {
    one_or_more(pqtoken()).map(|v| SubExpression::new(v))
}

//FIXME
pub fn pqtoken<'a>() -> impl Parser<'a, QuantifiedToken> {
    use quantifiers::quantifier;

    pair(token(), optional(quantifier()))
        .map(|(t, oq)|
            match oq {
                Some(q) => QuantifiedToken::Quantified(t, q),
                None    => QuantifiedToken::Singleton(t),
            })
}

pub fn token<'a>() -> impl Parser<'a, Token> {
    //macro time
    one_of!(Token, char_token(), indirect_match::indirect_match(), group(), anchor())
}

pub fn char_token<'a>() -> impl Parser<'a, Token> {
    char::char().map(|c| Token::Char(c))
}



pub mod indirect_match {
    //FIXME: CLEAN
    use super::*;

    pub fn indirect_match<'a>() -> impl Parser<'a, Token> {
        one_of!(IndirectMatch, wildcard(), char_class::char_class()).map(|im| Token::IndirectMatch(im))
    }

    pub fn wildcard<'a>() -> impl Parser<'a, IndirectMatch> {
        match_literal(".").map(|_u| IndirectMatch::WildCard)
    }
    
    pub mod char_class {
        //FIXME CLEANME
        use super::*;
        pub fn char_class<'a>() -> impl Parser<'a, IndirectMatch> {
            center(match_literal("["), class_inner::class_inner(), match_literal("]")).map(|cl| IndirectMatch::Class(cl))
        }

        pub mod class_inner {
            //FIXME CLEANME
            use super::*;

            pub fn class_inner<'a>() -> impl Parser<'a, CharClass> {
                pair(optional(match_literal("^")), raw_class_inner())
                    .map(|(inv, cls)|
                        match inv {
                            Some(_) => CharClass::Inverted(cls),
                            None => CharClass::Regular(cls),
                        })
            }

            pub fn raw_class_inner<'a>() -> impl Parser<'a, RawCharClass> {
                one_of!(RawCharClass, special_set::special_set(), character_range(), character_group())
            }

            pub fn character_group<'a>() -> impl Parser<'a, RawCharClass> {
                one_or_more(char::char()).map(|v| RawCharClass::CharGroup(v))
            }

            pub fn character_range<'a>() -> impl Parser<'a, RawCharClass> {
                outside(char::char(), match_literal("-"), char::char()).predicate(|(low, hi)| low <= hi).map(|(low, hi)| RawCharClass::CharRange(low, hi))
            }

            pub mod special_set {
                //FIXME CLEANME
                use super::*;

                pub fn special_set<'a>() -> impl Parser<'a, RawCharClass> {
                    right(match_literal("\\"), one_of!(CharSet, literal_set(), unicode_set::unicode_set())).map(|s| RawCharClass::SpecialSet(s))
                }

                pub fn literal_set<'a>() -> impl Parser<'a, CharSet> {
                    any_char.map(|c| map_literal_charset(c)).predicate(|o| o.is_some()).map(|o| o.expect("semantic error in literal_set parser"))
                }

                pub mod unicode_set {
                    //FIXME CLEANME
                    use super::*;

                    pub fn unicode_set<'a>() -> impl Parser<'a, CharSet> {
                        right(match_literal("p"), center(match_literal("{"), unicode_category(), match_literal("}")).map(|uc| CharSet::UnicodeCategory(uc)))
                    }

                    pub fn unicode_category<'a>() -> impl Parser<'a, UnicodeCategory> {
                        repeat_range(any_char.predicate(|c| *c != '}'), 1, Some(3)).map(|v| map_unicode_charset(&v.into_iter().collect::<String>())).predicate(|o| o.is_some()).map(|o| o.expect("semantic error in literal_set parser"))
                    }
                }
            }
        }
    }
}

pub fn group<'a>() -> impl Parser<'a, Token> {
    either(cgroup(), ncgroup()).map(|g| Token::Group(g))
}

//FIXME: Index
pub fn cgroup<'a>() -> impl Parser<'a, Group> {
    center(match_literal("("), lazy(expression), match_literal(")")).map(|e| Group::Capturing(0, Box::new(e)))
}

pub fn ncgroup<'a>() -> impl Parser<'a, Group> {
    center(match_literal("(?:"), lazy(expression), match_literal(")")).map(|e| Group::NonCapturing(Box::new(e)))
}

pub mod quantifiers {
    //FIXME: CLEAN THIS
    use super::*;
    //should all quantifiers be allowed to be lazy?
    pub fn quantifier<'a>() -> impl Parser<'a, Quantifier> {
        pair(raw::raw_quantifier(), optional(match_literal("?")))
            .map(|(r, l)|
                match l {
                    Some(_) => Quantifier::Lazy(r),
                    None => Quantifier::Greedy(r)
                })
    }
    pub mod raw {
        //FIXME: CLEAN THIS
        use super::*;

        pub fn raw_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            one_of!(RawQuantifier, range_quantifier(), kleene_star_quantifier(), plus_quantifier(), question_quantifier())
        }
    
        pub fn range_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            center(match_literal("{"), pair(number(), optional(right(match_literal(".."), optional(number())))), match_literal("}"))
                .map(|(l, o)| match o {
                    Some(h) => RawQuantifier::Range(l, h),
                    None => RawQuantifier::Exact(l),
                })
        }
    
        pub fn kleene_star_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("*").map(|_u| RawQuantifier::Kleene)
        }
    
        pub fn plus_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("+").map(|_u| RawQuantifier::Plus)
        }
    
        pub fn question_quantifier<'a>() -> impl Parser<'a, RawQuantifier> {
            match_literal("?").map(|_u| RawQuantifier::Possible)
        }
    }
}

pub fn anchor<'a>() -> impl Parser<'a, Token> {
    right( match_literal("\\"), raw_anchor() ).map(|a| Token::Anchor(a))
}

pub fn raw_anchor<'a>() -> impl Parser<'a, Anchor> {
    any_char.map(map_anchor).predicate(|o| o.is_some()).map(|o| o.expect("predicate should ensure not none"))
}