use crate::{
    parser::{PResult, PToken, PTokenKind, Parser},
    ptok,
};
use brim_ast::{
    Mut, Type,
    expr::{Expr, ExprKind},
    item::{Block, Ident, ItemKind, TypeAlias, TypeAliasValue},
    stmts::{Stmt, StmtKind},
    token::{BinOpToken, Delimiter, Orientation, TokenKind},
    ty::{Mutable, PrimitiveType, Ty, TyKind},
};

impl Parser {
    pub fn parse_type(&mut self) -> PResult<Ty> {
        let span = self.current().span;

        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            self.parse_ref()?
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            self.parse_ptr()?
        } else if self.current().is_keyword(Mut) {
            self.parse_mut()?
        } else if self
            .current()
            .is_delimiter(Delimiter::Bracket, Orientation::Open)
        {
            self.parse_array()?
        } else {
            let ident = self.parse_ident()?;

            if let Some(primitive) = self.is_primitive(ident)? {
                TyKind::Primitive(primitive)
            } else {
                TyKind::Ident {
                    ident,
                    generics: self.parse_argument_generics()?,
                }
            }
        };

        Ok(Ty {
            span,
            kind,
            id: self.new_id(),
        })
    }

    pub fn parse_ty_without_ident(&mut self) -> PResult<Option<Ty>> {
        let span = self.current().span;
        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            Some(self.parse_ref()?)
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            Some(self.parse_ptr()?)
        } else if self.current().is_keyword(Mut) {
            Some(self.parse_mut()?)
        } else if self
            .current()
            .is_delimiter(Delimiter::Bracket, Orientation::Open)
        {
            Some(self.parse_array()?)
        } else {
            let ident = self.parse_ident_without_err()?;

            if let Some(ident) = ident {
                if let Some(primitive) = self.is_primitive(ident)? {
                    Some(TyKind::Primitive(primitive))
                } else {
                    None
                }
            } else {
                None
            }
        };

        Ok(kind.map(|kind| Ty {
            span,
            kind,
            id: self.new_id(),
        }))
    }

    pub fn parse_mut(&mut self) -> PResult<TyKind> {
        self.eat_keyword(ptok!(Mut));

        Ok(TyKind::Mut(Box::new(self.parse_type()?)))
    }

    pub fn is_mut(&mut self) -> bool {
        if self.current().is_keyword(Mut) {
            self.eat_keyword(ptok!(Mut));
            true
        } else {
            false
        }
    }

    pub fn parse_ref(&mut self) -> PResult<TyKind> {
        let is_mut = self.is_mut();

        let ty = self.parse_type()?;
        Ok(TyKind::Ref(Box::new(ty), Mutable::from_bool(is_mut)))
    }

    pub fn parse_ptr(&mut self) -> PResult<TyKind> {
        let is_mut = self.is_mut();

        let ty = self.parse_type()?;
        Ok(TyKind::Ptr(Box::new(ty), Mutable::from_bool(is_mut)))
    }

    pub fn parse_array(&mut self) -> PResult<TyKind> {
        self.expect_obracket()?;

        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;
        let size_expr = self.parse_expr()?;
        let size_block = self.block_from_expr(size_expr);
        let size = self.new_expr(size_block.span, ExprKind::Block(size_block));

        self.expect_cbracket()?;
        Ok(TyKind::Array(Box::new(ty), size))
    }

    pub fn is_primitive(&self, ident: Ident) -> PResult<Option<PrimitiveType>> {
        Ok(PrimitiveType::try_from_string(ident.to_string()))
    }

    pub fn block_from_expr(&mut self, expr: Expr) -> Block {
        Block {
            id: self.new_id(),
            span: expr.span,
            stmts: vec![Stmt {
                id: self.new_id(),
                kind: StmtKind::Expr(expr.clone()),
                span: expr.span,
            }],
        }
    }

    pub fn parse_type_alias(&mut self) -> PResult<(Ident, ItemKind)> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Type));

        let ident = self.parse_ident()?;
        let generics = self.parse_generics()?;
        self.expect(TokenKind::Eq)?;

        let ty = if self.can_begin_comptime() {
            TypeAliasValue::Const(self.parse_expr()?)
        } else {
            TypeAliasValue::Ty(self.parse_type()?)
        };

        Ok((
            ident,
            ItemKind::TypeAlias(TypeAlias {
                span,
                generics,
                ident,
                ty,
            }),
        ))
    }
}
