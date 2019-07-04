use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: Kind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: Kind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[Line {}; Column {}]", self.line, self.column)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Kind {
    // idents and literals
    Ident(String),
    StringLiteral(String),
    FloatLiteral(f64),
    BoolLiteral(bool),

    // operator
    Plus,
    Minus,
    Slash,
    Asterisk,
    Caret,
    Modulo,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterThanEqual,
    LessThanEqual,
    Bang,

    // punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semicolon,

    // keyword
    Procedure,
    In,
    Out,

    // statement
    Break,
    If,
    Else,
    Loop,

    Illegal,
    EOF,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Kind::Ident(v) => write!(f, "Ident({})", v),
            Kind::StringLiteral(v) => write!(f, r#"StringLiteral("{}")"#, v),
            Kind::FloatLiteral(v) => write!(f, r#"FloatLiteral("{}")"#, v),
            Kind::BoolLiteral(v) => write!(f, r#"BoolLiteral("{}")"#, v),
            kind => write!(f, "{:?}", kind),
        }
    }
}
