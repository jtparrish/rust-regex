#[macro_use]

macro_rules! collect_parsers {
    ( $( $e:expr ),* ) => {
        vec![ $( BoxedParser::new($e) ),* ]
    };
}

macro_rules! one_of {
    ( $t:ty, $( $e:expr ),* ) => {
        one_of::<$t, Vec<BoxedParser<$t>>, BoxedParser<$t>>(collect_parsers![ $( BoxedParser::new($e) ),* ])
    };
}


use super::parsers::match_literal;
use super::BoxedParser;
use super::Parser;
#[cfg(test)]
mod tests {
    #[test]
    fn test_homoginize_parsers_macro() {
        use super::match_literal;
        use super::BoxedParser;
        use super::Parser;
        let v: Vec<BoxedParser<()>> = collect_parsers![match_literal("{"), match_literal("hello"), match_literal("world"), match_literal("}")];
        assert_eq!(v[0].parse("{"), Ok(( (), "" )));
        assert_eq!(v[1].parse("hello"), Ok(( (), "" )));
        assert_eq!(v[2].parse("world"), Ok(( (), "" )));
        assert_eq!(v[3].parse("}"), Ok(( (), "" )));
    }
}