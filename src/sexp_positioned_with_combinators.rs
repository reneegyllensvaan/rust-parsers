#![allow(dead_code)]

#[derive(Debug, PartialEq)]
pub enum SexpNode {
    Quote(Box<SexpNode>),
    List(Vec<SexpNode>),
    Number(f64),
    StrLit(String),
    Ident(String),
    Keyword(String),
}

pub type ParseResult<'a> = Result<Option<SexpNode>, String>;

pub type ParseFnResult<'a> = Result<Option<(SexpNode, usize)>, String>;

pub type Parser<'a> = dyn Fn(&'a str) -> ParseResult<'a> + 'a;

pub struct Mark {
    pos: usize,
    line: u32,
    col: u32,
}

pub struct Cursor<'a> {
    subject: &'a str,
    pos: usize,
    line: u32,
    col: u32,
}

impl<'a> Cursor<'a> {
    fn new(subject: &'a str) -> Self {
        Self {
            subject,
            pos: 0,
            line: 0,
            col: 0,
        }
    }

    fn mark(&self) -> (Mark, &'a str) {
        (
            Mark {
                pos: self.pos,
                line: self.line,
                col: self.col,
            },
            self.slice(),
        )
    }

    fn rewind(&mut self, mark: Mark) -> &'a str {
        self.pos = mark.pos;
        self.line = mark.line;
        self.col = mark.col;
        self.slice()
    }

    /// Advance the cursor position by a number of steps. This will scan for newlines too.
    fn advance(&mut self, steps: usize) -> &'a str {
        for (ix, c) in self.slice().char_indices() {
            if ix >= steps {
                break;
            }
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += 1;
            }
        }
        self.pos += steps;
        self.slice()
    }

    /// Advance the cursor to trim leading whitespace
    fn eat_whitespace(&mut self) -> &'a str {
        if let Some(n) = self.slice().find(|c: char| !c.is_whitespace()) {
            self.advance(n)
        } else {
            self.slice()
        }
    }

    fn finished(&self) -> bool {
        self.pos >= self.subject.len()
    }

    fn slice(&self) -> &'a str {
        &self.subject[self.pos..]
    }

    fn wrap(&mut self, mark: Mark, result: ParseResult<'a>) -> ParseResult<'a> {
        match result {
            Ok(None) => {
                self.rewind(mark);
            }
            Ok(Some(_)) => {}
            Err(e) => return Err(format!("{}:{}:\n\n    {}", self.line, self.col, e)),
        };
        result
    }

    fn try_parsefn<P>(&mut self, parser: P) -> ParseResult<'a>
    where
        P: Fn(&'a str) -> ParseFnResult<'a>,
    {
        let (mark, head) = self.mark();
        let result = parser(head);
        match result {
            Ok(None) => {
                self.rewind(mark);
                Ok(None)
            }
            Ok(Some((res, len))) => {
                self.advance(len);
                Ok(Some(res))
            }
            Err(e) => Err(format!("{}:{}:\n\n    {}", self.line, self.col, e)),
        }
    }
}

/// Returns true if a char is valid non-leading identifier member. Matches `[A-z0-9_-]`
fn is_ident_char(c: char) -> Result<bool, String> {
    if c.is_alphanumeric() {
        if c.is_ascii_alphanumeric() {
            Ok(true)
        } else {
            Err(format!("Invalid non-ascii character '{}' in identifier", c))
        }
    } else {
        Ok(false)
    }
}
// pub fn parse<'a>(input: &'a str) -> ParseResult<'a> {
//     let mut cursor = Cursor::new(input);
//     let result = parse_sexp(&mut cursor);
//     if cursor.slice().len() > 0 {
//         Err(format!("Trailing characters: {}", cursor.slice()))
//     } else {
//         result
//     }
// }

// pub fn parse_sexp<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, _) = cursor.mark();

//     let result = if let Some(parsed) = parse_str_lit(cursor)? {
//         Ok(Some(parsed))
//     } else if let Some(parsed) = parse_number(cursor)? {
//         Ok(Some(parsed))
//     } else if let Some(parsed) = parse_list(cursor)? {
//         Ok(Some(parsed))
//     } else if let Some(parsed) = parse_ident(cursor)? {
//         Ok(Some(parsed))
//     } else if let Some(parsed) = parse_keyword(cursor)? {
//         Ok(Some(parsed))
//     } else if let Some(parsed) = parse_quote(cursor)? {
//         Ok(Some(parsed))
//     } else {
//         Ok(None)
//     };
//     cursor.wrap(mark, result)
// }

// pub fn parse_number_fn<'a>(head: &'a str) -> ParseFnResult<'a> {
//     if let Some(_c @ '0'..='9') = head.chars().next() {
//         let mut end = 0;
//         for (ix, c) in head.char_indices() {
//             end = ix;
//             match c {
//                 '0'..='9' | '.' => continue,
//                 _ => break,
//             };
//         }
//         head[0..end]
//             .parse::<f64>()
//             .map(|num| Some((SexpNode::Number(num), end)))
//             .map_err(|_| format!("Invalid number literal '{}'", &head[0..end]))
//     } else {
//         Ok(None)
//     }
// }

// pub fn parse_str_lit<'a>(head: &'a str) -> ParseFnResult<'a> {
//     let mut chars = head.chars();
//     if let Some('"') = head.chars().next() {
//         let mut head = &head[1..];
//         let mut lit = String::new();
//         let mut last = ' ';
//         for ch in head.chars() {
//             match (last, ch) {
//                 ('\\', '"') => lit.push('"'),
//                 ('\\', c) => return Err(format!("Unknown escape sequence '\\{}'", c)),
//                 (_, '"') => break,
//                 (_, c) => lit.push(c),
//             }
//             last = ch;
//         }
//         head = &head[lit.len()..];
//         if let Some('"') = head.chars().next() {
//             Ok(Some((SexpNode::StrLit(lit), &head[1..])))
//         } else {
//             Err(format!("Unterminated string literal"))
//         }
//     } else {
//         Ok(None)
//     }
// }

// pub fn parse_list<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, head) = cursor.mark();
//     cursor.wrap(
//         mark,
//         if let Some('(') = head.chars().next() {
//             cursor.advance(1);
//             let mut values = Vec::<SexpNode>::new();
//             loop {
//                 let head = cursor.eat_whitespace();
//                 match parse_sexp(cursor)? {
//                     Some(n) => {
//                         values.push(n);
//                     }
//                     None => {
//                         return if let Some(')') = head.chars().next() {
//                             cursor.advance(1);
//                             Ok(Some(SexpNode::List(values)))
//                         } else {
//                             Err(format!("unexpected token '{}' at end of list", head))
//                         }
//                     }
//                 }
//             }
//         } else {
//             Ok(None)
//         },
//     )
// }

// pub fn parse_quote<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, head) = cursor.mark();
//     let result = if let Some('\'') = head.chars().next() {
//         cursor.advance(1);
//         match parse_sexp(cursor)? {
//             Some(n) => Ok(Some(SexpNode::Quote(Box::new(n)))),
//             None => Err(format!("Unexpected quoted token")),
//         }
//     } else {
//         Ok(None)
//     };
//     cursor.wrap(mark, result)
// }

// pub fn parse_keyword<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, head) = cursor.mark();
//     let result = if let Some(':') = head.chars().next() {
//         cursor.advance(1);
//         match parse_ident(cursor)? {
//             Some(SexpNode::Ident(n)) => Ok(Some(SexpNode::Keyword(n))),
//             _ => Err(format!("Expected keyword identifier following ':'")),
//         }
//     } else {
//         Ok(None)
//     };
//     cursor.wrap(mark, result)
// }

// pub fn parse_number<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, head) = cursor.mark();
//     let result = if let Some(_c @ '0'..='9') = head.chars().next() {
//         let mut end = 0;
//         for (ix, c) in head.char_indices() {
//             end = ix;
//             match c {
//                 '0'..='9' | '.' => cursor.advance(1),
//                 _ => break,
//             };
//         }
//         head[0..end]
//             .parse::<f64>()
//             .map(|num| Some(SexpNode::Number(num)))
//             .map_err(|_| format!("Invalid number token '{}'", 10))
//     } else {
//         Ok(None)
//     };
//     cursor.wrap(mark, result)
// }

// /// Tries to take an identifier out of the current head.
// pub fn parse_ident<'a>(cursor: &mut Cursor<'a>) -> ParseResult<'a> {
//     let (mark, head) = cursor.mark();
//     let result = if !head.starts_with(|c: char| c.is_ascii_alphabetic()) {
//         Ok(None)
//     } else {
//         let mut match_end = 0;
//         for (ix, c) in head.char_indices() {
//             match_end = ix;
//             if !is_ident_char(c)? {
//                 break;
//             }
//         }

//         cursor.advance(match_end);
//         Ok(Some(SexpNode::Ident(String::from(&head[0..match_end]))))
//     };
//     cursor.wrap(mark, result)
// }

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_parse_str_lit_valid() {
//         let input = "\"foo\"bar";
//         let mut cursor = Cursor::new(input);
//         assert_eq!(
//             parse_str_lit(&mut cursor),
//             Ok(Some(SexpNode::StrLit(String::from("foo"))))
//         );
//     }

//     #[test]
//     fn test_parse_str_lit_unterminated() {
//         let input = "\"foobar";
//         let mut cursor = Cursor::new(input);
//         assert_eq!(
//             parse_str_lit(&mut cursor),
//             Err(format!("0:0: Unterminated string literal"))
//         );
//     }

//     #[test]
//     fn test_parse_sexp() {
//         let input = "(foo bar baz 123.40 10 (arst 1234 ()) '(1 2 3))";
//         let mut cursor = Cursor::new(input);

//         assert_eq!(
//             parse_sexp(&mut cursor),
//             Ok(Some(SexpNode::List(vec![
//                 SexpNode::Ident(String::from("foo")),
//                 SexpNode::Ident(String::from("bar")),
//                 SexpNode::Ident(String::from("baz")),
//                 SexpNode::Number(123.4f64),
//                 SexpNode::Number(10f64),
//                 SexpNode::List(vec![
//                     SexpNode::Ident(String::from("arst")),
//                     SexpNode::Number(1234f64),
//                     SexpNode::List(vec![]),
//                 ]),
//                 SexpNode::Quote(Box::new(SexpNode::List(vec![
//                     SexpNode::Number(1f64),
//                     SexpNode::Number(2f64),
//                     SexpNode::Number(3f64),
//                 ]))),
//             ])))
//         );
//     }
// }
