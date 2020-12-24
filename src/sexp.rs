#![allow(dead_code)]

use super::{ParseResult, Parser};

#[derive(Debug, PartialEq)]
pub enum SexpNode {
    List(Vec<SexpNode>),
    Number(f64),
    StrLit(String),
    Ident(String),
}

fn eat_whitespace<'a>(head: &'a str) -> &'a str {
    head.trim_start()
}

pub fn parse<'a>(head: &'a str) -> ParseResult<'a, SexpNode> {
    fn parse_ident_number<'a>(s: &'a str) -> ParseResult<'a, SexpNode> {
        either(s, parse_ident, parse_number)
    }
    fn parse_terminal<'a>(s: &'a str) -> ParseResult<'a, SexpNode> {
        either(s, parse_ident_number, parse_str_lit)
    }
    let parse_value = |s| either(s, parse_terminal, parse_list);

    parse_value(head)
}

pub fn parse_str_lit<'a>(head: &'a str) -> ParseResult<'a, SexpNode> {
    if let Some('"') = head.chars().next() {
        let mut head = &head[1..];
        let mut lit = String::new();
        let mut last = ' ';
        for ch in head.chars() {
            match (last, ch) {
                ('\\', '"') => lit.push('"'),
                ('\\', c) => return Err(format!("Unknown escape sequence '\\{}'", c)),
                (_, '"') => break,
                (_, c) => lit.push(c),
            }
            last = ch;
        }
        head = &head[lit.len()..];
        if let Some('"') = head.chars().next() {
            Ok(Some((SexpNode::StrLit(lit), &head[1..])))
        } else {
            Err(format!("Unterminated string literal"))
        }
    } else {
        Ok(None)
    }
}

pub fn parse_list<'a>(head: &'a str) -> ParseResult<'a, SexpNode> {
    if let Some('(') = head.chars().next() {
        let mut head = &head[1..];
        let mut values = Vec::<SexpNode>::new();
        loop {
            head = eat_whitespace(head);
            match parse(head)? {
                Some((n, h)) => {
                    values.push(n);
                    head = h;
                }
                None => {
                    return if let Some(')') = head.chars().next() {
                        Ok(Some((SexpNode::List(values), &head[1..])))
                    } else {
                        Err(format!("unexpected token '{}' at end of list", head))
                    }
                }
            }
        }
    } else {
        Ok(None)
    }
}

pub fn parse_number<'a>(head: &'a str) -> ParseResult<'a, SexpNode> {
    if let Some(_c @ '0'..='9') = head.chars().next() {
        let mut end = 0;
        for (ix, c) in head.char_indices() {
            end = ix;
            match c {
                '0'..='9' | '.' => continue,
                _ => break,
            };
        }
        head[0..end]
            .parse::<f64>()
            .map(|num| Some((SexpNode::Number(num), &head[end..])))
            .map_err(|_| format!("Invalid number token '{}'", &head[0..end]))
    } else {
        Ok(None)
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

/// Tries to take an identifier out of the current head.
pub fn parse_ident<'a>(head: &'a str) -> ParseResult<'a, SexpNode> {
    match head.chars().next() {
        Some(c) if c.is_ascii_alphabetic() => {}
        _ => return Ok(None),
    }

    let mut match_end = 0;
    for (ix, c) in head.char_indices() {
        match_end = ix;
        if !is_ident_char(c)? {
            break;
        }
    }
    Ok(Some((
        SexpNode::Ident(String::from(&head[0..match_end])),
        &head[match_end..],
    )))
}

pub fn either<'a>(
    head: &'a str,
    prefer: Parser<'a, SexpNode>,
    otherwise: Parser<'a, SexpNode>,
) -> ParseResult<'a, SexpNode> {
    match prefer(head)? {
        Some(m) => Ok(Some(m)),
        None => otherwise(head),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_str_lit_valid() {
        assert_eq!(
            parse_str_lit("\"foo\"bar"),
            Ok(Some((SexpNode::StrLit(String::from("foo")), "bar")))
        );
    }

    #[test]
    fn test_parse_str_lit_unterminated() {
        assert_eq!(
            parse_str_lit("\"foobar"),
            Err(format!("Unterminated string literal"))
        );
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parse("(foo bar baz 123.40 10 (arst 1234 ()))"),
            Ok(Some((
                SexpNode::List(vec![
                    SexpNode::Ident(String::from("foo")),
                    SexpNode::Ident(String::from("bar")),
                    SexpNode::Ident(String::from("baz")),
                    SexpNode::Number(123.4f64),
                    SexpNode::Number(10f64),
                    SexpNode::List(vec![
                        SexpNode::Ident(String::from("arst")),
                        SexpNode::Number(1234f64),
                        SexpNode::List(vec![]),
                    ]),
                ]),
                ""
            )))
        );
    }
}
