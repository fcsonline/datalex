#![feature(proc_macro_hygiene)]
extern crate plex;

use std::io::Read;

// Example:
//
// [
//   :find ?title
//   :where
//   [?p :person/name "James Cameron"]
//   [?m :movie/director ?p]
//   [?m :movie/title ?title]
// ]
mod lexer {
    use plex::lexer;

    #[derive(Debug, Clone)]
    pub enum Token {
        Ident(String),
        Find, // :find
        Where, // :where
        QuestionId(String), // ?p
        Text(String), // "James Cameron"
        Placeholder, // _
        LBracket,
        RBracket,
        Whitespace,
    }

    lexer! {
        fn next_token(text: 'a) -> Token;

        r#"[ \t\r\n]+"# => Token::Whitespace,
        r#":find"# => Token::Find,
        r#":where"# => Token::Where,
        r#"\["# => Token::LBracket,
        r#"\]"# => Token::RBracket,
        r#"_"# => Token::Placeholder,
        r#"[a-zA-Z_:][a-zA-Z0-9_\/]*"# => Token::Ident(text.to_owned()),
        r#"\?[a-zA-Z_][a-zA-Z0-9_]*"# => Token::QuestionId(text.to_owned()),
        r#"\"[^\"]*\""# => Token::Text(text.to_owned()),

        r#"."# => panic!("unexpected character: {}", text),
    }

    pub struct Lexer<'a> {
        original: &'a str,
        remaining: &'a str,
    }

    impl<'a> Lexer<'a> {
        pub fn new(s: &'a str) -> Lexer<'a> {
            Lexer {
                original: s,
                remaining: s,
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Span {
        pub lo: usize,
        pub hi: usize,
    }

    impl<'a> Iterator for Lexer<'a> {
        type Item = (Token, Span);
        fn next(&mut self) -> Option<(Token, Span)> {
            loop {
                let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                    let lo = self.original.len() - self.remaining.len();
                    let hi = self.original.len() - new_remaining.len();
                    self.remaining = new_remaining;
                    (tok, Span { lo, hi })
                } else {
                    return None;
                };
                match tok {
                    Token::Whitespace => {
                        continue;
                    }
                    tok => {
                        return Some((tok, span));
                    }
                }
            }
        }
    }
}

mod ast {
    #[derive(Debug)]
    pub struct Program {
        pub query: Query
    }

    #[derive(Debug)]
    pub struct Query {
        pub bindings: Vec<Binding>,
        pub conditions: Vec<Condition>,
    }

    #[derive(Debug)]
    pub struct Binding {
        pub id: String,
    }

    #[derive(Debug)]
    pub struct Condition {
        pub id: Match,
        pub attr: Match,
        pub value: Match
    }

    #[derive(Debug)]
    pub enum Match {
        Placeholder,
        Binding(String),
        Value(String)
    }
}

mod parser {
    use ast::*;
    use lexer::Token::*;
    use lexer::*;
    use plex::parser;

    parser! {
        fn parse_(Token, Span);

        // combine two spans
        (a, b) {
            Span {
                lo: a.lo,
                hi: b.hi,
            }
        }

        program: Program {
            inner_query[q] => Program { query: q }
        }

        inner_query: Query {
            LBracket query[q] RBracket => q
        }

        query: Query {
            Find bindings[b] Where conditions[c] => Query {
                bindings: b,
                conditions: c
            }
        }

        bindings: Vec<Binding> {
            => vec![],
            bindings[mut bs] binding[b] => {
                bs.push(b);
                bs
            }
        }

        binding: Binding {
            QuestionId(b) => Binding {
                id: b
            },
        }

        conditions: Vec<Condition> {
            => vec![],
            conditions[mut cs] condition[c] => {
                cs.push(c);
                cs
            }
        }

        condition: Condition {
            LBracket mat[id] mat[attr] mat[value] RBracket => Condition {
                id: id,
                attr: attr,
                value: value
            }
        }

        mat: Match {
            Placeholder => Match::Placeholder,
            QuestionId(b) => Match::Binding(b),
            Ident(i) => Match::Value(i),
            Text(s) => Match::Value(s)
        }
    }

    pub fn parse<I: Iterator<Item = (Token, Span)>>(
        i: I,
    ) -> Result<Program, (Option<(Token, Span)>, &'static str)> {
        parse_(i)
    }
}

fn main() {
    let mut s = String::new();
    std::io::stdin().read_to_string(&mut s).unwrap();
    let lexer = lexer::Lexer::new(&s).inspect(|tok| eprintln!("tok: {:?}", tok));
    let program: ast::Program = parser::parse(lexer).unwrap();
    let condition = program.query.conditions.get(0).unwrap();

    let attr = match &condition.attr {
        ast::Match::Placeholder => "_",
        ast::Match::Binding(b) => &b,
        ast::Match::Value(ref s) => s
    };

    println!("FCS: {}", attr)
}
