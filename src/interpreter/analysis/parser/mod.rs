#[macro_use]
mod macros;

use crate::interpreter::analysis::{
    ast::*,
    errors::{AnalysisError, AnalysisResult},
    lexer::Lexer,
    token::*,
};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<AnalysisError>,
}

// TODO: I'm pretty sure I forgot to do something here so checking all the code
// is necessary to spot errors and gaps for improvement (not hard)
// TODO: I remember something, the errors must be handle. Right now everything
// returns AnalysisError::Generic. We need to add helpful and informative errors
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            lexer,
            current_token: Token::new(Kind::EOF, Span::new(1, 1)),
            peek_token: Token::new(Kind::EOF, Span::new(1, 1)),
            errors: vec![],
        };
        parser.advance();
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> AnalysisResult<Program> {
        let mut program: Program = vec![];
        while !current_token_is!(self, EOF) {
            let procedure = self.parse_procedure()?;
            program.push(procedure);
            self.advance();
        }
        Ok(program)
    }

    fn parse_procedure(&mut self) -> AnalysisResult<Statement> {
        expect_current_token!(self, Procedure)?;

        let ident = self.parse_ident()?;
        self.advance();

        let parameters = self.parse_parameters()?;
        self.advance();

        let block = self.parse_block()?;

        Ok(Statement::Procedure(ProcedureStatement {
            ident,
            parameters,
            block,
        }))
    }

    fn parse_block(&mut self) -> AnalysisResult<BlockStatement> {
        expect_current_token!(self, LBrace)?;

        let mut statements: Vec<Statement> = vec![];
        while !current_token_is!(self, RBrace) {
            let statement = match self.current_token.kind {
                Kind::Ident(_) => {
                    if peek_token_is!(self, LParen) {
                        self.parse_call_statement()?
                    } else if peek_token_is!(self, Equal) || peek_token_is!(self, LBracket) {
                        self.parse_assignment_statement()?
                    } else {
                        return Err(AnalysisError::Generic);
                    }
                }
                Kind::If => self.parse_if_statement()?,
                Kind::Loop => self.parse_loop_statement()?,
                Kind::Break => self.parse_break_if_statement()?,
                _ => return Err(AnalysisError::Generic),
            };
            statements.push(statement);
            self.advance();
        }

        Ok(statements)
    }

    fn parse_call_statement(&mut self) -> AnalysisResult<Statement> {
        let procedure_ident = self.parse_ident()?;

        expect_peek_token!(self, LParen)?;
        self.advance();

        if current_token_is!(self, RParen) {
            return Ok(Statement::Expression(ExpressionStatement::Call(
                CallExpression {
                    procedure_ident,
                    arguments: vec![],
                },
            )));
        }

        let arguments = self.parse_arguments()?;

        expect_peek_token!(self, RParen)?;
        self.advance();

        Ok(Statement::Expression(ExpressionStatement::Call(
            CallExpression {
                procedure_ident,
                arguments,
            },
        )))
    }

    fn parse_loop_statement(&mut self) -> AnalysisResult<Statement> {
        expect_current_token!(self, Loop)?;

        let block = self.parse_block()?;

        Ok(Statement::Loop(LoopStatement { block }))
    }

    fn parse_break_if_statement(&mut self) -> AnalysisResult<Statement> {
        expect_current_token!(self, Break)?;
        expect_current_token!(self, If)?;

        let condition = self.parse_expr(Precedence::Lowest)?;
        expect_peek_token!(self, Semicolon)?;

        Ok(Statement::BreakIf(BreakIfStatement { condition }))
    }

    fn parse_if_statement(&mut self) -> AnalysisResult<Statement> {
        expect_current_token!(self, If)?;

        let condition = self.parse_expr(Precedence::Lowest)?;

        self.advance();
        let consequence = self.parse_block()?;

        let alternative = if peek_token_is!(self, Else) {
            self.advance();
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };
        Ok(Statement::If(IfStatement {
            condition,
            consequence,
            alternative,
        }))
    }

    fn parse_assignment_statement(&mut self) -> AnalysisResult<Statement> {
        let ident = self.parse_ident()?;
        let index = if peek_token_is!(self, LBracket) {
            self.advance(); // current_token = LBracket
            self.advance(); // current_token = start of expression
            let index = self.parse_arguments()?;
            self.advance(); // current_token = RBracket
            Some(index)
        } else {
            None
        };

        expect_peek_token!(self, Equal)?;
        self.advance();

        let value = self.parse_expr(Precedence::Lowest)?;

        expect_peek_token!(self, Semicolon)?;

        Ok(Statement::Assignment(AssignmentStatement {
            ident,
            index,
            value,
        }))
    }

    fn parse_parameters(&mut self) -> AnalysisResult<Vec<Parameter>> {
        expect_current_token!(self, LParen)?;

        let mut params: Vec<Parameter> = vec![];
        
        if current_token_is!(self, RParen) {
            return Ok(params);
        }
        
        loop {
            let param = self.parse_parameter()?;
            params.push(param);
            if !peek_token_is!(self, Comma) {
                break;
            }
            self.advance();
            self.advance();
        }
        expect_peek_token!(self, RParen)?;
        Ok(params)
    }

    fn parse_parameter(&mut self) -> AnalysisResult<Parameter> {
        let mut is_in = if current_token_is!(self, In) {
            self.advance();
            true
        } else {
            false
        };

        let is_out = if current_token_is!(self, Out) {
            self.advance();
            true
        } else {
            false
        };
        // if there is not an attribute set, then the attribute «in»
        // is set by default
        is_in = if !is_in && !is_out { true } else { is_in };

        let ident = self.parse_ident()?;

        Ok(Parameter {
            is_in,
            is_out,
            ident,
        })
    }

    fn parse_ident(&mut self) -> AnalysisResult<IdentExpression> {
        let ident_value = get_current_token!(self, @Ident)?;
        Ok(IdentExpression { value: ident_value })
    }

    pub fn parse_expr(&mut self, precedence: Precedence) -> AnalysisResult<Expression> {
        let mut left = match self.current_token.kind {
            Kind::FloatLiteral(_) => self.parse_float_expr()?,
            Kind::BoolLiteral(_) => self.parse_bool_expr()?,
            Kind::StringLiteral(_) => self.parse_string_expr()?,
            Kind::Bang | Kind::Minus | Kind::Plus => self.parse_prefix_expr()?,
            Kind::Ident(_) => self.parse_ident_expr()?,
            Kind::LParen => self.parse_grouped_expr()?,
            _ => return Err(AnalysisError::Generic),
        };

        while !peek_token_is!(self, Semicolon) && precedence < self.peek_token_precedence() {
            match self.peek_token.kind {
                Kind::Plus
                | Kind::Minus
                | Kind::Asterisk
                | Kind::Slash
                | Kind::Caret
                | Kind::Equal
                | Kind::NotEqual
                | Kind::GreaterThan
                | Kind::GreaterThanEqual
                | Kind::LessThan
                | Kind::LessThanEqual
                | Kind::And
                | Kind::Or => {
                    self.advance();
                    left = self.parse_infix_expr(left)?
                }
                Kind::LBracket => {
                    self.advance();
                    left = self.parse_index_expr(left)?
                }
                _ => return Err(AnalysisError::Generic),
            }
        }

        Ok(left)
    }

    fn parse_prefix_expr(&mut self) -> AnalysisResult<Expression> {
        let kind = match self.current_token.kind {
            Kind::Bang => PrefixKind::Not,
            Kind::Plus => PrefixKind::Plus,
            Kind::Minus => PrefixKind::Minus,
            _ => return Err(AnalysisError::Generic),
        };
        self.advance();
        Ok(Expression::Prefix(PrefixExpression {
            kind,
            right: Box::new(self.parse_expr(Precedence::Prefix)?),
        }))
    }

    fn parse_infix_expr(&mut self, left: Expression) -> AnalysisResult<Expression> {
        let infix_precedence = self.current_token_precedence();
        let kind = match self.current_token.kind {
            Kind::Plus => InfixKind::Plus,
            Kind::Minus => InfixKind::Minus,
            Kind::Asterisk => InfixKind::Multiply,
            Kind::Slash => InfixKind::Divide,
            Kind::Caret => InfixKind::Power,
            Kind::Equal => InfixKind::Equal,
            Kind::NotEqual => InfixKind::NotEqual,
            Kind::GreaterThan => InfixKind::GreaterThan,
            Kind::GreaterThanEqual => InfixKind::GreaterThanEqual,
            Kind::LessThan => InfixKind::LessThan,
            Kind::LessThanEqual => InfixKind::LessThanEqual,
            Kind::And => InfixKind::And,
            Kind::Or => InfixKind::Or,
            _ => return Err(AnalysisError::Generic),
        };
        self.advance();
        let right = self.parse_expr(infix_precedence)?;
        Ok(Expression::Infix(InfixExpression {
            left: Box::new(left),
            kind,
            right: Box::new(right),
        }))
    }

    fn parse_index_expr(&mut self, ident: Expression) -> AnalysisResult<Expression> {
        let ident = match ident {
            Expression::Ident(v) => v,
            _ => return Err(AnalysisError::Generic),
        };
        expect_current_token!(self, LBracket)?;
        if current_token_is!(self, RBracket) {
            return Err(AnalysisError::Generic);
        }
        let indices = self.parse_arguments()?;
        expect_peek_token!(self, RBracket)?;
        Ok(Expression::Index(IndexExpression { ident, indices }))
    }

    fn parse_arguments(&mut self) -> AnalysisResult<Vec<Expression>> {
        let mut args: Vec<Expression> = vec![];
        loop {
            let arg = self.parse_expr(Precedence::Lowest)?;
            args.push(arg);
            if !peek_token_is!(self, Comma) {
                break;
            }
            self.advance();
            self.advance();
        }
        Ok(args)
    }

    fn parse_ident_expr(&mut self) -> AnalysisResult<Expression> {
        Ok(Expression::Ident(self.parse_ident()?))
    }

    fn parse_grouped_expr(&mut self) -> AnalysisResult<Expression> {
        expect_current_token!(self, LParen)?;

        let expr = self.parse_expr(Precedence::Lowest)?;
        expect_peek_token!(self, RParen)?;
        Ok(expr)
    }

    fn parse_float_expr(&mut self) -> AnalysisResult<Expression> {
        let float = get_current_token!(self, @FloatLiteral)?;
        Ok(Expression::FloatLiteral(float))
    }

    fn parse_string_expr(&mut self) -> AnalysisResult<Expression> {
        let string = get_current_token!(self, @StringLiteral)?;
        Ok(Expression::StringLiteral(string))
    }

    fn parse_bool_expr(&mut self) -> AnalysisResult<Expression> {
        let bool = get_current_token!(self, @BoolLiteral)?;
        Ok(Expression::BoolLiteral(bool))
    }

    fn advance(&mut self) {
        self.current_token = self.peek_token.clone();
        if let Some(next_token) = self.lexer.next_token() {
            self.peek_token = next_token;
        }
    }

    fn current_token_precedence(&self) -> Precedence {
        Self::token_precedence(&self.current_token.kind)
    }

    fn peek_token_precedence(&self) -> Precedence {
        Self::token_precedence(&self.peek_token.kind)
    }

    fn token_precedence(token: &Kind) -> Precedence {
        match token {
            Kind::Equal | Kind::NotEqual => Precedence::Equals,
            Kind::LessThan | Kind::LessThanEqual | Kind::GreaterThan | Kind::GreaterThanEqual => {
                Precedence::LessGreater
            }
            Kind::Plus | Kind::Minus => Precedence::Sum,
            Kind::Slash | Kind::Asterisk => Precedence::Product,
            Kind::LParen => Precedence::Call,
            Kind::Caret => Precedence::Power,
            Kind::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

// TODO: Improve tests. Some tests must be improved and some of them don't
// even have an assert. Those tests without an assert, were tested by eye.
// I printed the result and check them with my two eyes. Sorry
#[cfg(test)]
mod tests {
    use super::*;

    fn parse_parameter_helper(input: &str) -> Parameter {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        parser
            .parse_parameter()
            .expect("Error on parse_parameter_helper")
    }

    #[test]
    fn test_parameter_in_out() {
        let result = parse_parameter_helper("in out parAmX1");
        let expected_result = Parameter {
            is_in: true,
            is_out: true,
            ident: IdentExpression {
                value: String::from("parAmX1"),
            },
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_parameter_in() {
        let result = parse_parameter_helper("in param");
        let expected_result = Parameter {
            is_in: true,
            is_out: false,
            ident: IdentExpression {
                value: String::from("param"),
            },
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_parameter_out() {
        let result = parse_parameter_helper("out PARAM");
        let expected_result = Parameter {
            is_in: false,
            is_out: true,
            ident: IdentExpression {
                value: String::from("PARAM"),
            },
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_parameter_default_attribute() {
        let result = parse_parameter_helper("LMAO_PARAM");
        let expected_result = Parameter {
            is_in: true,
            is_out: false,
            ident: IdentExpression {
                value: String::from("LMAO_PARAM"),
            },
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_ident() {
        let mut parser = Parser::new(Lexer::new("hola123"));
        let result = parser
            .parse_ident()
            .expect("Expected identifier but got a ParseError");
        let expected_result = IdentExpression {
            value: String::from("hola123"),
        };
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_ident_error() {
        let mut parser = Parser::new(Lexer::new("123notallowed"));
        let result = parser
            .parse_ident()
            .expect_err("Expected error on ident but got Ok!");
    }

    #[test]
    fn test_parameters() {
        let mut parser = Parser::new(Lexer::new("(in a, out b, in out c, d)"));
        let result = parser
            .parse_parameters()
            .expect("Expected parameters but got a ParseError");
        let expected_result = vec![
            Parameter {
                is_in: true,
                is_out: false,
                ident: IdentExpression {
                    value: String::from("a"),
                },
            },
            Parameter {
                is_in: false,
                is_out: true,
                ident: IdentExpression {
                    value: String::from("b"),
                },
            },
            Parameter {
                is_in: true,
                is_out: true,
                ident: IdentExpression {
                    value: String::from("c"),
                },
            },
            Parameter {
                is_in: true,
                is_out: false,
                ident: IdentExpression {
                    value: String::from("d"),
                },
            },
        ];
        assert_eq!(result, expected_result);
    }
    
    #[test]
    fn test_empty_parameters() {
        let mut parser = Parser::new(Lexer::new("()"));
        let result = parser
            .parse_parameters()
            .expect("Expected parameters but got a ParseError");
        let expected_result = vec![];
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_procedure_empty_block() {
        let input = "procedure my_procedure(in x, out y) {}";
        let mut parser = Parser::new(Lexer::new(input));
        let result = parser
            .parse_procedure()
            .expect("Error parsing procedure with empty block");
        let expected_result = Statement::Procedure(ProcedureStatement {
            ident: IdentExpression {
                value: String::from("my_procedure"),
            },
            parameters: vec![
                Parameter {
                    ident: IdentExpression {
                        value: String::from("x"),
                    },
                    is_in: true,
                    is_out: false,
                },
                Parameter {
                    ident: IdentExpression {
                        value: String::from("y"),
                    },
                    is_in: false,
                    is_out: true,
                },
            ],
            block: vec![],
        });
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_float_infix_expr() {
        let mut parser = Parser::new(Lexer::new("1+2*3+4^2"));
        parser.parse_expr(Precedence::Lowest).unwrap();
    }

    #[test]
    fn test_not_prefix_expr() {
        let mut parser = Parser::new(Lexer::new("!true"));
        let result = parser
            .parse_expr(Precedence::Lowest)
            .expect("Got error on parsing not prefix expression");
        let expected_result = Expression::Prefix(PrefixExpression {
            kind: PrefixKind::Not,
            right: Box::new(Expression::BoolLiteral(true)),
        });
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_minus_prefix_expr() {
        let mut parser = Parser::new(Lexer::new("-2"));
        let result = parser
            .parse_expr(Precedence::Lowest)
            .expect("Got error on parsing minus prefix expression");
        let expected_result = Expression::Prefix(PrefixExpression {
            kind: PrefixKind::Minus,
            right: Box::new(Expression::FloatLiteral(2.0)),
        });
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_plus_prefix_expr() {
        let mut parser = Parser::new(Lexer::new("+4.2"));
        let result = parser
            .parse_expr(Precedence::Lowest)
            .expect("Got error on parsing plus prefix expression");
        let expected_result = Expression::Prefix(PrefixExpression {
            kind: PrefixKind::Plus,
            right: Box::new(Expression::FloatLiteral(4.2)),
        });
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_index_expression() {
        let mut parser = Parser::new(Lexer::new("hola[1, 3, 10]"));
        let result = parser
            .parse_expr(Precedence::Lowest)
            .expect("Got error on parsing index expression");
        let expected_result = Expression::Index(IndexExpression {
            ident: IdentExpression {
                value: String::from("hola"),
            },
            indices: vec![
                Expression::FloatLiteral(1.0),
                Expression::FloatLiteral(3.0),
                Expression::FloatLiteral(10.0),
            ],
        });
        assert_eq!(result, expected_result);
    }

    #[test]
    fn test_call_statement() {
        let mut parser = Parser::new(Lexer::new("byebye()"));
        let result = parser
            .parse_call_statement()
            .expect("Got error on parsing call statement");
    }

    #[test]
    fn test_assignment_statement() {
        let mut parser = Parser::new(Lexer::new("x = 10 + 20;"));
        let result = parser
            .parse_assignment_statement()
            .expect("Got error on parsing assignment statement");
    }

    #[test]
    fn test_assignment_with_index_statement() {
        let mut parser = Parser::new(Lexer::new("x[1*(2+1)] = 10 + 20;"));
        let result = parser
            .parse_assignment_statement()
            .expect("Got error on parsing assignment statement");
    }

    #[test]
    fn test_loop_statement() {
        let mut parser = Parser::new(Lexer::new("loop {x = 1; break if (x = 10); x = x + 1;}"));
        let result = parser
            .parse_loop_statement()
            .expect("Got error on parsing loop statement");
    }

    #[test]
    fn test_if_statement() {
        let mut parser = Parser::new(Lexer::new(r#"if ((1 + 1) = 2) { print("hey"); }"#));
        let result = parser
            .parse_if_statement()
            .expect("Got error on parsing if statement");
    }

    #[test]
    fn test_if_else_statement() {
        let mut parser = Parser::new(Lexer::new(
            r#"if ((1 + 2) = 2) { print("what"); } else { print("hey"); }"#,
        ));
        let result = parser
            .parse_if_statement()
            .expect("Got error on parsing if statement");
    }
}
