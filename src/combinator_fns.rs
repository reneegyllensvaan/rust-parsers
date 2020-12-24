//#![allow(dead_code)]

//use std::cell::RefCell;

//// use super::{ParseResult};

//// Currently unused, using a hand-written parser since it's currently just sexprs: (notes)
////   - Should maybe use a proper result type? `std::ops::Try` is still experimental, so
////     that means no try operator
////   - Currently not working for mutually recursive parse rules, because combinator rules take
////     ownership of their terminal rules. Try using a ref cell to share them.
////

//#[derive(Debug)]
//pub enum SexpNode {
//    List(Vec<SexpNode>),
//    Number(f64),
//    StrLit(String),
//    Atom(String),
//}

//pub type ParseResult<'a> = Result<Option<(SexpNode, &'a str)>, String>;

//pub type ParseFn<'a> = dyn Fn(&'a str) -> ParseResult<'a> + 'a;

//impl<'a> Parser<'a> {
//    // fn new<P>(parser: P) -> Parser<'a>
//    // where
//    //     P: Fn(&'a str) -> ParseResult<'a> + 'a,
//    // {
//    //     Parser {
//    //         parser: Box::new(parser),
//    //     }
//    // }

//    fn new<P>(parser: P) -> Box<ParseFn<'a>>
//    where
//        P: Fn(&'a str) -> ParseResult<'a> + 'a,
//    {
//        Box::new(parser)
//    }

//    fn parse(&self, head: &'a str) -> ParseResult<'a> {
//        (self.parser)(head)
//    }
//}

//pub fn parse_literal<'a /*, Prod*/>(lit: &'static str, /*, production: &'a Prod*/) -> ParseFn<'a>
//where
//    // L: 'a,
//    //     Prod: Fn() -> SexpNode + 'a,
//{
//    Parser::new(move |head: &'a str| {
//        Ok(match head.get(0..lit.len()) {
//            Some(s) if s == lit => Some((SexpNode::Number(10f64), &head[lit.len()..])),
//            _ => None,
//        })
//    })
//}

//pub fn coalesce<'a>(prefer: &'a ParseFn<'a>, otherwise: &'a ParseFn<'a>) -> (fn()) {
//    move |head: &'a str| match prefer.parse(head)? {
//        Some(res) => Ok(Some(res)),
//        None => otherwise.parse(head),
//    }
//}

//#[cfg(test)]
//mod tests {
//    use super::*;

//    #[test]
//    fn test_build_shared_boxed_parser() {
//        // let spam = parse_literal("spam", &|| SexpNode::Atom(String::from("spam")));
//        let spam = parse_literal("spam");
//        let parser = coalesce(&spam, &spam);
//        // println!("{:?}", parser.parse("spam span"));
//    }
//}

//// pub fn concat<'a, Prod>(
////     left: &'a Parser<'a>,
////     right: &'a Parser<'a>,
////     production: &'a Prod,
//// ) -> Parser<'a>
//// where
////     Prod: Fn(SexpNode, SexpNode) -> SexpNode + 'a,
//// {
////     Parser::new(move |head| match left.parse(head)? {
////         Some((l, half_tail)) => match right.parse(half_tail)? {
////             Some((r, tail)) => Ok(Some((production(l, r), tail))),
////             None => Ok(None),
////         },
////         None => Ok(None),
////     })
//// }
