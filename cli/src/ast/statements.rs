use std::fmt::{Debug, Display, Formatter};
use indexmap::IndexMap;
use crate::ast::{Ast, ExprId, GetSpan, StmtId};
use crate::error::span::TextSpan;
use crate::lexer::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub id: StmtId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StmtKind {
    Expr(ExprId),
    Use(Use),
    Block(Block),
    If(If),
    Return(Return),
    Fn(Fn),
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

#[derive(Clone, Debug, PartialEq)]
pub struct StructField {
    pub ident: Token,
    pub type_annotation: TypeAnnotation,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitDef {
    pub trait_token: Token,
    pub name: Token,
    pub methods: Vec<Fn>,
    pub public: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructImpl {
    pub impl_token: Token,
    pub struct_name: Token,
    pub methods: Vec<Fn>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TraitImpl {
    pub impl_token: Token,
    pub trait_name: Token,
    pub for_token: Token,
    pub struct_name: Token,
    pub methods: Vec<Fn>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Loop {
    pub loop_token: Token,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct While {
    pub while_token: Token,
    pub condition: ExprId,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Throw {
    pub value: ExprId,
    pub token: Token,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Try {
    pub try_token: Token,
    pub try_block: Block,
    pub error_ident: Token,
    pub catch_block: Block,
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
    pub fn as_function(&self) -> &Fn {
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
    pub is_rest: bool,
}

impl GetSpan for FnParam {
    fn span(&self, ast: &Ast) -> TextSpan {
        TextSpan::combine(vec![self.ident.span.clone(), self.type_annotation.span(ast)]).unwrap()
    }
}

impl FnParam {
    pub fn new(ident: Token, type_annotation: TypeAnnotation, is_rest: bool) -> Self {
        Self {
            ident,
            type_annotation,
            is_rest,
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
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    String,
    Int,
    Float,
    Char,
    Bool,
    Vec,
    Object,
    Anytype,
    Void,
    Custom(String),
}

impl Display for TypeKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeKind::String => write!(f, "string"),
            TypeKind::Int => write!(f, "int"),
            TypeKind::Float => write!(f, "float"),
            TypeKind::Char => write!(f, "char"),
            TypeKind::Bool => write!(f, "bool"),
            TypeKind::Vec => write!(f, "vec"),
            TypeKind::Object => write!(f, "object"),
            TypeKind::Anytype => write!(f, "anytype"),
            TypeKind::Void => write!(f, "void"),
            TypeKind::Custom(name) => write!(f, "{}", name),
        }
    }
}

impl TypeKind {
    pub fn from_str(s: &str) -> Self {
        match s {
            "string" => TypeKind::String,
            "int" => TypeKind::Int,
            "float" => TypeKind::Float,
            "char" => TypeKind::Char,
            "bool" => TypeKind::Bool,
            "vec" => TypeKind::Vec,
            "object" => TypeKind::Object,
            "anytype" => TypeKind::Anytype,
            "void" => TypeKind::Void,
            _ => TypeKind::Custom(s.to_string()),
        }
    }
}

impl TypeAnnotation {
    pub fn is_any(&self) -> bool {
        self.kind == TypeKind::Anytype
    }

    pub fn is_generic(&self) -> bool {
        self.generics.len() > 0
    }

    pub fn match_generic(&self, generic: TypeKind, args: Vec<TypeKind>) -> bool {
        let generics_names = self
            .generics
            .iter()
            .map(|g| g.kind.clone())
            .collect::<Vec<TypeKind>>();

        self.kind == generic && generics_names == args
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
pub struct Fn {
    pub fn_token: Token,
    pub name: String,
    pub params: Vec<FnParam>,
    pub body: Block,
    pub public: bool,
    pub return_type: Option<TypeAnnotation>,
    pub is_static: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct If {
    pub if_token: Token,
    pub condition: ExprId,
    pub then_block: Block,
    pub else_ifs: Vec<ElseBlock>,
    pub else_block: Option<ElseBlock>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ElseBlock {
    pub condition: ExprId,
    pub block: Block,
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
    pub stmts: Vec<Stmt>,
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
