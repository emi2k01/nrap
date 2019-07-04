use super::token::*;

pub struct Lexer {
    input: Vec<char>,
    current_char: Option<char>,
    // current_char position in the input
    position: usize,
    line: usize,
    column: usize,
    eof: bool,
}

// TODO: Handle lexical errors
impl Lexer {
    pub fn new(input: &str) -> Self {
        let input: Vec<char> = input.chars().collect();
        let mut lexer = Self {
            input,
            current_char: None,
            position: 0,
            line: 1,
            column: 1,
            eof: false,
        };
        lexer.current_char = lexer.input.get(0).cloned();
        lexer
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_whitespace();

        let current_char = match self.current_char {
            Some(v) => v,
            None => {
                if !self.eof {
                    self.eof = true;
                    return Some(Token::new(Kind::EOF, Span::new(self.line, self.column)));
                }
                return None;
            }
        };

        let start_column = self.column;
        use Kind::*;
        let next_token_kind = {
            match current_char {
                '+' => Plus,
                '-' => Minus,
                '*' => Asterisk,
                '/' => Slash,
                '^' => Caret,
                '%' => Modulo,
                '=' => Equal,
                '(' => LParen,
                ')' => RParen,
                '{' => LBrace,
                '}' => RBrace,
                '[' => LBracket,
                ']' => RBracket,
                ',' => Comma,
                ';' => Semicolon,
                '>' => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        GreaterThanEqual
                    } else {
                        GreaterThan
                    }
                }
                '<' => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        LessThanEqual
                    } else {
                        LessThan
                    }
                }
                '!' => {
                    if self.peek_char() == Some(&'=') {
                        self.read_char();
                        NotEqual
                    } else {
                        Bang
                    }
                }
                _ => {
                    // check for keywords and identifiers (true, false procedure, x)
                    let new_token_kind = if current_char.is_ascii_alphabetic()
                        || current_char == '$'
                        || current_char == '_'
                    {
                        self.lex_word()
                    } else if current_char.is_ascii_digit() || current_char == '.' {
                        // check for number literals (.1, 10)
                        self.lex_number()
                    } else if current_char == '"' {
                        // check for string literals ("yea hey, cow says \"yea\"")
                        self.lex_string()
                    } else {
                        self.read_char();
                        Illegal
                    };
                    return Some(Token::new(
                        new_token_kind,
                        Span::new(self.line, start_column),
                    ));
                }
            }
        };
        self.read_char();
        Some(Token::new(
            next_token_kind,
            Span::new(self.line, start_column),
        ))
    }

    fn lex_word(&mut self) -> Kind {
        // TODO: Handle the dollar sign here instead of relying in the next_token
        // function, so we can use the lex_word function by itself
        let mut word = String::new();

        // This function is only called if there's a valid self.current_char
        // so an error should not happen here
        let current_char = self
            .current_char
            .expect("Error in keyword lexer. This should never happen");
        word.push(current_char.to_owned());
        self.read_char();

        while let Some(current_char) = self.current_char {
            if current_char.is_ascii_whitespace() {
                break;
            }
            if current_char.is_alphanumeric() || current_char == '_' {
                word.push(current_char.to_owned());
                self.read_char();
            } else {
                break;
            }
        }

        match word.as_str() {
            "true" => Kind::BoolLiteral(true),
            "false" => Kind::BoolLiteral(false),
            "procedure" => Kind::Procedure,
            "break" => Kind::Break,
            "if" => Kind::If,
            "else" => Kind::Else,
            "loop" => Kind::Loop,
            "in" => Kind::In,
            "out" => Kind::Out,
            _ => Kind::Ident(word),
        }
    }

    fn lex_number(&mut self) -> Kind {
        let mut got_point = false;
        let mut number_str = String::new();
        while let Some(current_char) = self.current_char {
            if current_char.is_ascii_whitespace() {
                break;
            }
            if current_char == '.' {
                // if the current_char is a point, then check if we already got
                // a point to avoid numbers like 10.0.1
                if got_point {
                    self.read_char();
                    return Kind::Illegal;
                } else {
                    // If we got a valid point, check if the peek_char
                    // is a valid number to avoid numbers made of only a point
                    if let Some(peek_char) = self.peek_char() {
                        if peek_char.is_ascii_digit() || peek_char.is_whitespace() {
                            number_str.push(current_char.to_owned());
                            got_point = true;
                            self.read_char();
                        } else {
                            return Kind::Illegal;
                        }
                    } else {
                        return Kind::Illegal;
                    }
                }
            } else if current_char.is_ascii_digit() {
                number_str.push(current_char.to_owned());
                self.read_char();
            } else {
                break;
            }
        }
        // if we get an error, it means there's an error in the lexer itself
        let number = number_str
            .parse::<f64>()
            .expect("Error in number lexer. This should never happen");
        Kind::FloatLiteral(number)
    }

    fn lex_string(&mut self) -> Kind {
        self.read_char();
        let mut escaping = false;
        let mut string = String::new();
        while let Some(current_char) = self.current_char {
            if current_char == '\\' {
                escaping = true;
                self.read_char();
            } else if current_char == '"' {
                if escaping {
                    string.push(current_char.to_owned());
                    self.read_char();
                } else {
                    self.read_char();
                    break;
                }
                escaping = false;
            } else {
                string.push(current_char.to_owned());
                self.read_char();
                escaping = false;
            }
        }
        Kind::StringLiteral(string)
    }

    fn skip_whitespace(&mut self) {
        while let Some(current_char) = self.current_char {
            match current_char {
                ' ' | '\t' => {
                    self.read_char();
                }
                '\n' => {
                    self.read_char();
                    self.line += 1;
                    self.column = 1;
                }
                _ => break,
            }
        }
    }

    fn read_char(&mut self) {
        self.position += 1;
        self.column += 1;
        self.current_char = self.input.get(self.position).cloned();
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.get(self.position + 1)
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! _str {
        ($string:expr) => {
            String::from($string)
        };
    }

    #[test]
    fn test_punctuation_operators() {
        let input = "+-*/=,;==!(){}[]!=<>+<=+>=";
        let lexer = Lexer::new(&input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Plus, Span::new(1, 1)),
            Token::new(Kind::Minus, Span::new(1, 2)),
            Token::new(Kind::Asterisk, Span::new(1, 3)),
            Token::new(Kind::Slash, Span::new(1, 4)),
            Token::new(Kind::Equal, Span::new(1, 5)),
            Token::new(Kind::Comma, Span::new(1, 6)),
            Token::new(Kind::Semicolon, Span::new(1, 7)),
            Token::new(Kind::Equal, Span::new(1, 8)),
            Token::new(Kind::Equal, Span::new(1, 9)),
            Token::new(Kind::Bang, Span::new(1, 10)),
            Token::new(Kind::LParen, Span::new(1, 11)),
            Token::new(Kind::RParen, Span::new(1, 12)),
            Token::new(Kind::LBrace, Span::new(1, 13)),
            Token::new(Kind::RBrace, Span::new(1, 14)),
            Token::new(Kind::LBracket, Span::new(1, 15)),
            Token::new(Kind::RBracket, Span::new(1, 16)),
            Token::new(Kind::NotEqual, Span::new(1, 17)),
            Token::new(Kind::LessThan, Span::new(1, 19)),
            Token::new(Kind::GreaterThan, Span::new(1, 20)),
            Token::new(Kind::Plus, Span::new(1, 21)),
            Token::new(Kind::LessThanEqual, Span::new(1, 22)),
            Token::new(Kind::Plus, Span::new(1, 24)),
            Token::new(Kind::GreaterThanEqual, Span::new(1, 25)),
            Token::new(Kind::EOF, Span::new(1, 27)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_numbers() {
        let input = ".1 0.2 14 24. 123.456 .21";
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::FloatLiteral(0.1), Span::new(1, 1)),
            Token::new(Kind::FloatLiteral(0.2), Span::new(1, 4)),
            Token::new(Kind::FloatLiteral(14.0), Span::new(1, 8)),
            Token::new(Kind::FloatLiteral(24.0), Span::new(1, 11)),
            Token::new(Kind::FloatLiteral(123.456), Span::new(1, 15)),
            Token::new(Kind::FloatLiteral(0.21), Span::new(1, 23)),
            Token::new(Kind::EOF, Span::new(1, 26)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_keywords() {
        let input = "procedure if else true false loop break variable";
        let lexer = Lexer::new(&input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Procedure, Span::new(1, 1)),
            Token::new(Kind::If, Span::new(1, 11)),
            Token::new(Kind::Else, Span::new(1, 14)),
            Token::new(Kind::BoolLiteral(true), Span::new(1, 19)),
            Token::new(Kind::BoolLiteral(false), Span::new(1, 24)),
            Token::new(Kind::Loop, Span::new(1, 30)),
            Token::new(Kind::Break, Span::new(1, 35)),
            Token::new(Kind::Ident(String::from("variable")), Span::new(1, 41)),
            Token::new(Kind::EOF, Span::new(1, 49)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_bool() {
        let input = "false true";
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::BoolLiteral(false), Span::new(1, 1)),
            Token::new(Kind::BoolLiteral(true), Span::new(1, 7)),
            Token::new(Kind::EOF, Span::new(1, 11)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_string() {
        let input = r#" "applé" "®egistered" "\"title\"" "#;
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        // using two-byte wide characters
        let expected_result = vec![
            Token::new(Kind::StringLiteral(_str!("applé")), Span::new(1, 2)),
            Token::new(Kind::StringLiteral(_str!("®egistered")), Span::new(1, 10)),
            Token::new(Kind::StringLiteral(_str!(r#""title""#)), Span::new(1, 23)),
            Token::new(Kind::EOF, Span::new(1, 35)),
        ];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_program() {
        let input = r#"
            procedure main() {
                x = a+b;
                y = c/d;
                z = x+y;
                x = "string";
                empty = "";
                x = -1. + 2.25;
                v[0] = 1;

                loop {
                    break if(x == b);
                }

                if (x > y) {
                } else if (x == y) {
                } else {
                    if (true == false) {
                    }
                }
            }
        "#;
        let lexer = Lexer::new(input);

        let result: Vec<Token> = lexer.collect();

        let expected_result = vec![
            Token::new(Kind::Procedure, Span::new(2, 13)),
            Token::new(Kind::Ident(_str!("main")), Span::new(2, 23)),
            Token::new(Kind::LParen, Span::new(2, 27)),
            Token::new(Kind::RParen, Span::new(2, 28)),
            Token::new(Kind::LBrace, Span::new(2, 30)),
            Token::new(Kind::Ident(_str!("x")), Span::new(3, 17)),
            Token::new(Kind::Equal, Span::new(3, 19)),
            Token::new(Kind::Ident(_str!("a")), Span::new(3, 21)),
            Token::new(Kind::Plus, Span::new(3, 22)),
            Token::new(Kind::Ident(_str!("b")), Span::new(3, 23)),
            Token::new(Kind::Semicolon, Span::new(3, 24)),
            Token::new(Kind::Ident(_str!("y")), Span::new(4, 17)),
            Token::new(Kind::Equal, Span::new(4, 19)),
            Token::new(Kind::Ident(_str!("c")), Span::new(4, 21)),
            Token::new(Kind::Slash, Span::new(4, 22)),
            Token::new(Kind::Ident(_str!("d")), Span::new(4, 23)),
            Token::new(Kind::Semicolon, Span::new(4, 24)),
            Token::new(Kind::Ident(_str!("z")), Span::new(5, 17)),
            Token::new(Kind::Equal, Span::new(5, 19)),
            Token::new(Kind::Ident(_str!("x")), Span::new(5, 21)),
            Token::new(Kind::Plus, Span::new(5, 22)),
            Token::new(Kind::Ident(_str!("y")), Span::new(5, 23)),
            Token::new(Kind::Semicolon, Span::new(5, 24)),
            Token::new(Kind::Ident(_str!("x")), Span::new(6, 17)),
            Token::new(Kind::Equal, Span::new(6, 19)),
            Token::new(Kind::StringLiteral(_str!("string")), Span::new(6, 21)),
            Token::new(Kind::Semicolon, Span::new(6, 29)),
            Token::new(Kind::Ident(_str!("empty")), Span::new(7, 17)),
            Token::new(Kind::Equal, Span::new(7, 23)),
            Token::new(Kind::StringLiteral(_str!("")), Span::new(7, 25)),
            Token::new(Kind::Semicolon, Span::new(7, 27)),
            Token::new(Kind::Ident(_str!("x")), Span::new(8, 17)),
            Token::new(Kind::Equal, Span::new(8, 19)),
            Token::new(Kind::Minus, Span::new(8, 21)),
            Token::new(Kind::FloatLiteral(1.0), Span::new(8, 22)),
            Token::new(Kind::Plus, Span::new(8, 25)),
            Token::new(Kind::FloatLiteral(2.25), Span::new(8, 27)),
            Token::new(Kind::Semicolon, Span::new(8, 31)),
            Token::new(Kind::Ident(_str!("v")), Span::new(9, 17)),
            Token::new(Kind::LBracket, Span::new(9, 18)),
            Token::new(Kind::FloatLiteral(0.0), Span::new(9, 19)),
            Token::new(Kind::RBracket, Span::new(9, 20)),
            Token::new(Kind::Equal, Span::new(9, 22)),
            Token::new(Kind::FloatLiteral(1.0), Span::new(9, 24)),
            Token::new(Kind::Semicolon, Span::new(9, 25)),
            Token::new(Kind::Loop, Span::new(11, 17)),
            Token::new(Kind::LBrace, Span::new(11, 22)),
            Token::new(Kind::Break, Span::new(12, 21)),
            Token::new(Kind::If, Span::new(12, 27)),
            Token::new(Kind::LParen, Span::new(12, 29)),
            Token::new(Kind::Ident(_str!("x")), Span::new(12, 30)),
            Token::new(Kind::Equal, Span::new(12, 32)),
            Token::new(Kind::Equal, Span::new(12, 33)),
            Token::new(Kind::Ident(_str!("b")), Span::new(12, 35)),
            Token::new(Kind::RParen, Span::new(12, 36)),
            Token::new(Kind::Semicolon, Span::new(12, 37)),
            Token::new(Kind::RBrace, Span::new(13, 17)),
            Token::new(Kind::If, Span::new(15, 17)),
            Token::new(Kind::LParen, Span::new(15, 20)),
            Token::new(Kind::Ident(_str!("x")), Span::new(15, 21)),
            Token::new(Kind::GreaterThan, Span::new(15, 23)),
            Token::new(Kind::Ident(_str!("y")), Span::new(15, 25)),
            Token::new(Kind::RParen, Span::new(15, 26)),
            Token::new(Kind::LBrace, Span::new(15, 28)),
            Token::new(Kind::RBrace, Span::new(16, 17)),
            Token::new(Kind::Else, Span::new(16, 19)),
            Token::new(Kind::If, Span::new(16, 24)),
            Token::new(Kind::LParen, Span::new(16, 27)),
            Token::new(Kind::Ident(_str!("x")), Span::new(16, 28)),
            Token::new(Kind::Equal, Span::new(16, 30)),
            Token::new(Kind::Equal, Span::new(16, 31)),
            Token::new(Kind::Ident(_str!("y")), Span::new(16, 33)),
            Token::new(Kind::RParen, Span::new(16, 34)),
            Token::new(Kind::LBrace, Span::new(16, 36)),
            Token::new(Kind::RBrace, Span::new(17, 17)),
            Token::new(Kind::Else, Span::new(17, 19)),
            Token::new(Kind::LBrace, Span::new(17, 24)),
            Token::new(Kind::If, Span::new(18, 21)),
            Token::new(Kind::LParen, Span::new(18, 24)),
            Token::new(Kind::BoolLiteral(true), Span::new(18, 25)),
            Token::new(Kind::Equal, Span::new(18, 30)),
            Token::new(Kind::Equal, Span::new(18, 31)),
            Token::new(Kind::BoolLiteral(false), Span::new(18, 33)),
            Token::new(Kind::RParen, Span::new(18, 38)),
            Token::new(Kind::LBrace, Span::new(18, 40)),
            Token::new(Kind::RBrace, Span::new(19, 21)),
            Token::new(Kind::RBrace, Span::new(20, 17)),
            Token::new(Kind::RBrace, Span::new(21, 13)),
            Token::new(Kind::EOF, Span::new(22, 9)),
        ];
        assert_eq!(result, expected_result);
    }
}
