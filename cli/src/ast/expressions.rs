use indexmap::IndexMap;
use std::fmt::{Display, Formatter};
use crate::ast::{Ast, ExprId, GetSpan};
use crate::ast::types::TypeKind;
use crate::error::span::TextSpan;
use crate::lexer::tokens::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayExpr {
    pub exprs: Vec<ExprId>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LiteralType {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Char(char),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Literal {
    pub token: Token,
    pub value: LiteralType,
}

impl Literal {
    pub fn new(token: Token, value: LiteralType) -> Self {
        Literal { token, value }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOpKind {
    /// Addition operator (`+`).
    Plus,
    /// Subtraction operator (`-`).
    Minus,
    /// Multiplication operator (`*`).
    Multiply,
    /// Division operator (`/`).
    Divide,
    /// Exponentiation operator (`**`).
    Power,
    /// Modulo operator (`%`).
    Modulo,
    // Bitwise operators
    /// Bitwise AND operator (`&`).
    BitwiseAnd,
    /// Bitwise OR operator (`|`).
    BitwiseOr,
    /// Bitwise XOR operator (`^`).
    BitwiseXor,
    /// Bitwise shift left operator (`<<`).
    ShiftLeft,
    /// Bitwise shift right operator (`>>`).
    ShiftRight,
    // Relational operators
    /// Equality operator (`==`).
    Equals,
    /// Less-than operator (`<`).
    LessThan,
    /// Less-than-or-equal operator (`<=`).
    LessThanOrEqual,
    /// Greater-than operator (`>`).
    GreaterThan,
    /// Greater-than-or-equal operator (`>=`).
    GreaterThanOrEqual,
    /// Equality operator (`==`).
    EqualsEquals,
    /// Inequality operator (`!=`).
    BangEquals,
    // Logical operators
    /// Logical AND operator (`&&`).
    And,
    /// Logical OR operator (`||`).
    Or,
    // Increment/Decrement operators
    /// Increment operator (`++`).
    Increment,
    /// Decrement operator (`--`).
    Decrement,
}

impl BinOpKind {
    pub fn is_number_operator(&self) -> bool {
        match self {
            BinOpKind::Plus
            | BinOpKind::Minus
            | BinOpKind::Multiply
            | BinOpKind::Divide
            | BinOpKind::Power
            | BinOpKind::Modulo
            | BinOpKind::Increment
            | BinOpKind::Decrement
            | BinOpKind::BitwiseAnd
            | BinOpKind::BitwiseOr
            | BinOpKind::BitwiseXor
            | BinOpKind::ShiftLeft
            | BinOpKind::ShiftRight => true,
            _ => false,
        }
    }

    pub fn is_boolean_operator(&self) -> bool {
        match self {
            BinOpKind::Equals
            | BinOpKind::LessThan
            | BinOpKind::LessThanOrEqual
            | BinOpKind::GreaterThan
            | BinOpKind::GreaterThanOrEqual
            | BinOpKind::EqualsEquals
            | BinOpKind::BangEquals
            | BinOpKind::And
            | BinOpKind::Or => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub left: ExprId,
    pub operator: BinOpKind,
    pub right: ExprId,
}

impl GetSpan for Binary {
    fn span(&self, ast: &Ast) -> TextSpan {
        let left = ast.query_expr(self.left).span(ast);
        let right = ast.query_expr(self.right).span(ast);

        TextSpan::combine(vec![left, right]).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub operator: UnOperator,
    pub expr: ExprId,
    pub token: Token,
}

impl GetSpan for Unary {
    fn span(&self, ast: &Ast) -> TextSpan {
        TextSpan::combine(vec![self.operator.token.span.clone(), ast.query_expr(self.expr).span(ast)]).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub ident: String,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parenthesized {
    pub expr: ExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: String,
    pub args: Vec<ExprId>,
    pub token: Token,
}

impl GetSpan for CallExpr {
    /// Returns the source span of the function call expression.
    fn span(&self, ast: &Ast) -> TextSpan {
        // TODO: get the span of the closing parenthesis
        let callee_span = self.token.span.clone();

        let args_span: Vec<TextSpan> = self.args.iter().map(|arg| ast.query_expr(*arg).span(ast)).collect();
        let args_span = TextSpan::combine(args_span);

        let mut spans = vec![callee_span];

        if args_span.is_some() {
            spans.push(args_span.unwrap());
        }

        TextSpan::combine(spans).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignOperator {
    /// Assignment operator (`=`).
    Assign,
    /// Addition assignment operator (`+=`).
    PlusEquals,
    /// Subtraction assignment operator (`-=`).
    MinusEquals,
    /// Multiplication assignment operator (`*=`).
    MultiplyEquals,
    /// Division assignment operator (`/=`).
    DivideEquals,
}

impl Display for AssignOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AssignOperator::Assign => write!(f, "="),
            AssignOperator::PlusEquals => write!(f, "+="),
            AssignOperator::MinusEquals => write!(f, "-="),
            AssignOperator::MultiplyEquals => write!(f, "*="),
            AssignOperator::DivideEquals => write!(f, "/="),
        }
    }
}

impl AssignOperator {
    pub fn from_token_kind(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Equals => AssignOperator::Assign,
            TokenKind::PlusEquals => AssignOperator::PlusEquals,
            TokenKind::MinusEquals => AssignOperator::MinusEquals,
            TokenKind::MultiplyEquals => AssignOperator::MultiplyEquals,
            TokenKind::DivideEquals => AssignOperator::DivideEquals,
            _ => todo!("Proper error"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assign {
    pub left: ExprId,
    pub op: AssignOperator,
    pub right: ExprId,
}

impl GetSpan for Assign {
    fn span(&self, ast: &Ast) -> TextSpan {
        let left = ast.query_expr(self.left).span(ast);
        let right = ast.query_expr(self.right).span(ast);

        TextSpan::combine(vec![left, right]).unwrap()
    }
}

/// Enum representing unary operator kinds (e.g., `-`, `~`).
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnOpKind {
    /// Minus operator (`-`).
    Minus,
    /// Bitwise NOT operator (`~`).
    BitwiseNot,
    /// Logical NOT operator (`!`).
    LogicalNot,
}

impl Display for UnOpKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOpKind::Minus => write!(f, "-"),
            UnOpKind::BitwiseNot => write!(f, "~"),
            UnOpKind::LogicalNot => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnOperator {
    pub kind: UnOpKind,
    pub token: Token,
}

impl UnOperator {
    pub fn new(kind: UnOpKind, token: Token) -> Self {
        UnOperator { kind, token }
    }
}

#[derive(Debug, Clone)]
pub struct BinOperator {
    pub kind: BinOpKind,
    pub token: Token,
}

impl BinOperator {
    pub fn new(kind: BinOpKind, token: Token) -> Self {
        BinOperator { kind, token }
    }

    /// Returns the precedence of the operator.
    /// Higher numbers indicate higher precedence.
    pub fn precedence(&self) -> u8 {
        match self.kind {
            // Highest precedence
            BinOpKind::Power => 20,
            BinOpKind::Multiply | BinOpKind::Divide | BinOpKind::Modulo => 19,
            BinOpKind::Plus | BinOpKind::Minus => 18,
            BinOpKind::ShiftLeft | BinOpKind::ShiftRight => 17,
            BinOpKind::BitwiseAnd => 16,
            BinOpKind::BitwiseXor => 15,
            BinOpKind::BitwiseOr => 14,
            // Relational operators
            BinOpKind::LessThan
            | BinOpKind::LessThanOrEqual
            | BinOpKind::GreaterThan
            | BinOpKind::GreaterThanOrEqual => 13,
            // Equality operators
            BinOpKind::Equals | BinOpKind::EqualsEquals | BinOpKind::BangEquals => 12,
            // Logical operators
            BinOpKind::And => 11,
            BinOpKind::Or => 10,
            // Increment/Decrement operators
            BinOpKind::Increment | BinOpKind::Decrement => 9,
        }
    }

    /// Returns the associativity of the operator.
    /// Operators can be either left-associative or right-associative.
    pub fn associativity(&self) -> BinOpAssociativity {
        match self.kind {
            BinOpKind::Power => BinOpAssociativity::Right,
            _ => BinOpAssociativity::Left,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOpAssociativity {
    Left,
    Right,
}

#[derive(Debug, Clone)]
#[derive(PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprId,
    pub ty: TypeKind,
}

impl Expr {
    pub fn new(kind: ExprKind, id: ExprId, ty: TypeKind) -> Self {
        Expr { kind, id, ty }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Literal(Literal),
    Binary(Binary),
    Unary(Unary),
    Variable(Variable),
    Parenthesized(Parenthesized),
    Call(CallExpr),
    Assign(Assign),
    Array(ArrayExpr),
    Access(AccessExpr),
    Null(Token),
    StructConstructor(StructConstructor),
    ThenElse(ThenElse),
    Object(ObjectExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjectExpr {
    pub fields: IndexMap<String, ExprId>,
    pub braces: (Token, Token),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThenElse {
    pub condition: ExprId,
    pub then_expr: ExprId,
    pub else_expr: ExprId,
    pub then_token: Token,
    pub else_token: Token,
}

impl GetSpan for ThenElse {
    fn span(&self, ast: &Ast) -> TextSpan {
        let condition_span = ast.query_expr(self.condition).span(ast);
        let then_span = ast.query_expr(self.then_expr).span(ast);
        let else_span = ast.query_expr(self.else_expr).span(ast);

        TextSpan::combine(vec![condition_span, then_span, else_span]).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructConstructor {
    pub name: String,
    pub fields: IndexMap<String, ExprId>,
    pub token: Token,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AccessKind {
    Field(ExprId),
    Index(ExprId),
    StaticMethod(ExprId),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AccessExpr {
    pub base: ExprId,
    pub access: AccessKind,
    pub token: Token,
}

impl GetSpan for AccessExpr {
    fn span(&self, ast: &Ast) -> TextSpan {
        let base_span = ast.query_expr(self.base).span(ast);
        let access_span = match &self.access {
            AccessKind::Field(_) => self.token.span.clone(), // Span includes the '.' and the field name
            AccessKind::Index(index_expr) => {
                TextSpan::combine(vec![self.token.span.clone(), ast.query_expr(*index_expr).span(ast)]).unwrap()
            } // Span includes '[' , index, and ']'
            AccessKind::StaticMethod(method) => {
                TextSpan::combine(vec![self.token.span.clone(), ast.query_expr(*method).span(ast)]).unwrap()
            }
        };
        TextSpan::combine(vec![base_span, access_span]).unwrap()
    }
}

impl GetSpan for Expr {
    fn span(&self, ast: &Ast) -> TextSpan {
        match &self.kind {
            ExprKind::Literal(l) => l.clone().token.span,
            ExprKind::Binary(b) => {
                let left = ast.query_expr(b.left).span(ast);
                let right = ast.query_expr(b.right).span(ast);
                TextSpan::combine(vec![left, right]).unwrap()
            }
            ExprKind::Unary(u) => u.clone().token.span,
            ExprKind::Variable(v) => v.clone().token.span,
            ExprKind::Parenthesized(p) => ast.query_expr(p.expr).span(ast),
            ExprKind::Call(c) => c.clone().token.span,
            ExprKind::Assign(a) => {
                let left = ast.query_expr(a.left).span(ast);
                let right = ast.query_expr(a.right).span(ast);
                TextSpan::combine(vec![left, right]).unwrap()
            }
            ExprKind::Array(v) => {
                let spans: Vec<TextSpan> = v.exprs.iter().map(|e| ast.query_expr(*e).span(ast)).collect();
                TextSpan::combine(spans).unwrap()
            }
            ExprKind::Access(a) => ast.query_expr(a.base).span(ast),
            ExprKind::Null(t) => t.span.clone(),
            ExprKind::StructConstructor(s) => s.token.span.clone(),
            ExprKind::ThenElse(t) => t.span(ast),
            ExprKind::Object(o) => {
                TextSpan::combine(vec![o.braces.0.span.clone(), o.braces.1.span.clone()]).unwrap()
            }
        }
    }
}
