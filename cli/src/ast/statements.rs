use std::fmt::{Debug, Display, Formatter};
use indexmap::IndexMap;
use crate::ast::{Ast, ExprId, GetSpan, StmtId};
use crate::ast::types::TypeKind;
use crate::error::span::TextSpan;
use crate::lexer::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: StmtId,
}

impl Stmt {
    pub fn new(kind: StmtKind, id: StmtId) -> Self {
        Self { kind, id }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    Expr(ExprId),
    Use(Use),
    Block(Block),
    If(If),
    Return(Return),
    Fn(Function),
    Let(Let),
    Try(Try),
    Break(Token),
    Continue(Token),
    Loop(Loop),
    While(While),
    Struct(Struct),
    TraitDef(TraitDef),
    StructImpl(StructImpl),
    TraitImpl(TraitImpl),
    Const(Const),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Const {
    pub expr: ExprId,
    pub ident: Token,
    pub public: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Struct {
    pub struct_token: Token,
    pub name: Token,
    pub fields: IndexMap<String, StructField>,
    pub public: bool,
    pub impls: Vec<StructImpl>,
    pub trait_impls: Vec<TraitImpl>,
}

impl Struct {
    pub fn span(&self) -> TextSpan {
        TextSpan::combine(vec![self.struct_token.span.clone(), self.name.span.clone()]).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub ident: Token,
    pub type_annotation: TypeAnnotation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitDef {
    pub trait_token: Token,
    pub name: Token,
    pub methods: Vec<StmtId>,
    pub public: bool,
}

impl TraitDef {
    pub(crate) fn span(&self) -> TextSpan {
        TextSpan::combine(vec![self.trait_token.span.clone(), self.name.span.clone()]).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructImpl {
    pub impl_token: Token,
    pub struct_name: Token,
    pub methods: Vec<StmtId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitImpl {
    pub impl_token: Token,
    pub trait_name: Token,
    pub for_token: Token,
    pub struct_name: Token,
    pub methods: Vec<StmtId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub token: Token,
    pub block: StmtId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub while_token: Token,
    pub condition: ExprId,
    pub block: StmtId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Throw {
    pub value: ExprId,
    pub token: Token,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Try {
    pub token: Token,
    pub expr: ExprId,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Let {
    pub ident: Token,
    pub initializer: ExprId,
    pub type_annotation: Option<TypeAnnotation>,
}

impl GetSpan for Let {
    fn span(&self, ast: &Ast) -> TextSpan {
        let mut spans = vec![self.ident.span.clone()];

        if let Some(type_annotation) = &self.type_annotation {
            spans.push(type_annotation.span(ast));
        }

        spans.push(
            ast.query_expr(self.initializer)
                .span(ast)
        );

        TextSpan::combine(spans).unwrap()
    }
}

impl Stmt {
    pub fn as_function(&self) -> &Function {
        match &self.kind {
            StmtKind::Fn(f) => f,
            _ => panic!("Expected function"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FnParam {
    pub ident: Token,
    pub type_annotation: TypeAnnotation,
}

impl GetSpan for FnParam {
    fn span(&self, ast: &Ast) -> TextSpan {
        TextSpan::combine(vec![self.ident.span.clone(), self.type_annotation.span(ast)]).unwrap()
    }
}

impl FnParam {
    pub fn new(ident: Token, type_annotation: TypeAnnotation) -> Self {
        Self {
            ident,
            type_annotation,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAnnotation {
    pub separator: Option<Token>,
    pub token_name: Option<Token>,
    pub kind: TypeKind,
    pub is_nullable: bool,
    pub module_id: Option<String>,
    pub generics: Vec<TypeAnnotation>,
    pub can_be_error: bool,
    pub error_type: Option<Token>,
}

impl TypeAnnotation {
    pub fn is_generic(&self) -> bool {
        self.generics.len() > 0
    }
}

impl GetSpan for TypeAnnotation {
    fn span(&self, ast: &Ast) -> TextSpan {
        let mut spans = vec![];

        if let Some(token) = &self.separator {
            spans.push(token.span.clone());
        }

        if let Some(token) = &self.token_name {
            spans.push(token.span.clone());
        }

        for generic in &self.generics {
            spans.push(generic.span(ast));
        }

        TextSpan::combine(spans).unwrap()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Function {
    pub fn_token: Token,
    pub name: String,
    pub params: Vec<FnParam>,
    pub body: StmtId,
    pub public: bool,
    pub return_type: Option<TypeAnnotation>,
    pub is_static: bool,
}

impl Function {
    pub fn span(&self) -> TextSpan {
        let spans = vec![self.fn_token.span.clone()];

        TextSpan::combine(spans).unwrap()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub if_token: Token,
    pub condition: ExprId,
    pub then_block: StmtId,
    pub else_ifs: Vec<ElseBlock>,
    pub else_block: Option<ElseBlock>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseBlock {
    pub condition: ExprId,
    pub block: StmtId,
    pub else_if: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Use {
    pub use_token: Token,
    pub from: Token,
    pub items: Vec<Token>,
}

#[derive(Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Block")
            .field("stmts", &self.stmts.len())
            .finish()
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Return {
    pub return_token: Token,
    pub expr: Option<ExprId>,
}
