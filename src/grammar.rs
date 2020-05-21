pub struct Expression {
    alt: Alternation,
}

impl Expression {
    fn new(choices: Vec<SubExpression>) -> Self {
        Expression{ alt: Alternation::new(choices) }
    }
}

pub struct Alternation {
    choices: Vec<SubExpression>
}

impl Alternation {
    fn new(choices: Vec<SubExpression>) -> Self {
        Alternation{ choices: choices }
    }
}

pub struct SubExpression {
    tokens: Vec<QuantifiedToken>
}

impl SubExpression {
    fn new(tokens: Vec<QuantifiedToken>) -> Self {
        SubExpression{ tokens: tokens }
    }
}

pub enum QuantifiedToken {
    Singleton(Token),
    Quantified(Token, Quantifier)
}

pub enum Token {
    Char(char),
    IndirectMatch(IndirectMatch),
    Group(Group),
    Anchor(Anchor),
    Unimpl,
}

pub enum Anchor {
    Begin,
    End,
}

pub enum Group {
    Capturing(usize, Box<Expression>),
    NonCapturing(Box<Expression>),
}

pub enum Quantifier {
    Lazy(RawQuantifier),
    Greedy(RawQuantifier),
}

pub enum RawQuantifier {
    Kleene,
    Plus,
    Possible,
    Exact(usize),
    Range(usize, Option<usize>),
}

pub enum IndirectMatch {
    WildCard,
    Class(CharClass),
}

pub enum CharClass {
    Regular(RawCharClass),
    Inverted(RawCharClass),
}

pub enum RawCharClass {
    CharGroup(Vec<char>),
    CharRange(char, char),
    SpecialSet(CharSet),
}

pub enum CharSet {
    Word,
    WhiteSpace,
    DecimalDigit,
    UnicodeCategory(UnicodeCategory),
}

pub enum UnicodeCategory {
    Punctuation,

}

//TODO
fn not_reserved(_c: &char) -> bool {
    true
}

//TODO
fn escapable(_c: &char) -> bool {
    false
}

fn map_literal_charset(c: char) -> Option<CharSet> {
    match c {
        'w' => Some(CharSet::Word),
        's' => Some(CharSet::WhiteSpace),
        'd' => Some(CharSet::DecimalDigit),
         _  => None,
    }
}

fn map_unicode_charset(s: &str) -> Option<UnicodeCategory> {
    match s {
        "P"  => Some(UnicodeCategory::Punctuation),
        "Lt" => Some(UnicodeCategory::Punctuation),
        "Ll" => Some(UnicodeCategory::Punctuation),
        "N"  => Some(UnicodeCategory::Punctuation),
        "S"  => Some(UnicodeCategory::Punctuation),
         _   => None,
    }
}

fn map_anchor(c: char) -> Option<Anchor> {
    match c {
        '^' => Some(Anchor::Begin),
        '$' => Some(Anchor::End),
         _  => None,
    }
}