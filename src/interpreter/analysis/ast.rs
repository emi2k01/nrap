pub type BlockStatement = Vec<Statement>;
pub type Program = BlockStatement;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Procedure(ProcedureStatement),
    Assignment(AssignmentStatement),
    If(IfStatement),
    Loop(LoopStatement),
    BreakIf(BreakIfStatement),
    Expression(ExpressionStatement),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ExpressionStatement {
    Call(CallExpression),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Ident(IdentExpression),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Call(CallExpression),
    Index(IndexExpression),
    Infix(InfixExpression),
    Prefix(PrefixExpression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct IdentExpression {
    pub value: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct CallExpression {
    pub procedure_ident: IdentExpression,
    pub arguments: Vec<Expression>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum InfixKind {
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    And,
    Or,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub kind: InfixKind,
    pub right: Box<Expression>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum PrefixKind {
    Plus,
    Minus,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PrefixExpression {
    pub kind: PrefixKind,
    pub right: Box<Expression>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Parameter {
    pub ident: IdentExpression,
    pub is_in: bool,
    pub is_out: bool,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ProcedureStatement {
    pub ident: IdentExpression,
    pub parameters: Vec<Parameter>,
    pub block: BlockStatement,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexExpression {
    pub ident: IdentExpression,
    pub indices: Vec<Expression>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct AssignmentStatement {
    pub ident: IdentExpression,
    pub index: Option<Vec<Expression>>,
    pub value: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfStatement {
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BreakIfStatement {
    pub condition: Expression,
}

#[derive(PartialEq, Debug, Clone)]
pub struct LoopStatement {
    pub block: BlockStatement,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Power,
    Prefix,
    Call,
    Index,
}
