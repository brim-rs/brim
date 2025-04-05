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
        let mut ty = self.parse_type_core()?;

        ty = self.apply_type_modifiers(ty)?;

        Ok(ty)
    }

    fn parse_type_core(&mut self) -> PResult<Ty> {
        let span = self.current().span;

        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            let is_mut = self.is_mut();
            let inner_ty = self.parse_type_core()?;
            TyKind::Ref(Box::new(inner_ty), Mutable::from_bool(is_mut))
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            let is_mut = self.is_mut();
            let inner_ty = self.parse_type_core()?;
            TyKind::Ptr(Box::new(inner_ty), Mutable::from_bool(is_mut))
        } else if self.current().is_keyword(Mut) {
            self.eat_keyword(ptok!(Mut));
            let inner_ty = self.parse_type_core()?;
            TyKind::Mut(Box::new(inner_ty))
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

    fn apply_type_modifiers(&mut self, mut ty: Ty) -> PResult<Ty> {
        let span = ty.span;

        // Handle array/vector types
        if self
            .current()
            .is_delimiter(Delimiter::Bracket, Orientation::Open)
            && self
                .next()
                .is_delimiter(Delimiter::Bracket, Orientation::Close)
        {
            self.expect_obracket()?;
            self.expect_cbracket()?;
            ty = Ty {
                span,
                kind: TyKind::Vec(Box::new(ty)),
                id: self.new_id(),
            };
        }

        // Handle Result type
        if self.current().kind == TokenKind::Bang {
            self.eat(TokenKind::Bang);
            let err_ty = self.parse_type()?;
            ty = Ty {
                span,
                kind: TyKind::Result(Box::new(ty), Box::new(err_ty)),
                id: self.new_id(),
            };
        }

        // Handle Option type
        if self.current().kind == TokenKind::QuestionMark {
            self.eat(TokenKind::QuestionMark);
            ty = Ty {
                span,
                kind: TyKind::Option(Box::new(ty)),
                id: self.new_id(),
            };
        }

        Ok(ty)
    }

    pub fn parse_ty_without_ident(&mut self) -> PResult<Option<Ty>> {
        let ty_opt = self.parse_type_core_without_ident()?;

        if let Some(ty) = ty_opt {
            let ty_with_modifiers = self.apply_type_modifiers(ty)?;
            Ok(Some(ty_with_modifiers))
        } else {
            Ok(None)
        }
    }

    fn parse_type_core_without_ident(&mut self) -> PResult<Option<Ty>> {
        let span = self.current().span;

        let kind_opt = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            let is_mut = self.is_mut();
            let inner_ty_opt = self.parse_type_core_without_ident()?;
            inner_ty_opt.map(|inner_ty| TyKind::Ref(Box::new(inner_ty), Mutable::from_bool(is_mut)))
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            let is_mut = self.is_mut();
            let inner_ty_opt = self.parse_type_core_without_ident()?;
            inner_ty_opt.map(|inner_ty| TyKind::Ptr(Box::new(inner_ty), Mutable::from_bool(is_mut)))
        } else if self.current().is_keyword(Mut) {
            self.eat_keyword(ptok!(Mut));
            let inner_ty_opt = self.parse_type_core_without_ident()?;
            inner_ty_opt.map(|inner_ty| TyKind::Mut(Box::new(inner_ty)))
        } else {
            let ident_opt = self.parse_ident_without_err()?;

            if let Some(ident) = ident_opt {
                if let Some(primitive) = self.is_primitive(ident)? {
                    Some(TyKind::Primitive(primitive))
                } else {
                    None
                }
            } else {
                None
            }
        };

        Ok(kind_opt.map(|kind| Ty {
            span,
            kind,
            id: self.new_id(),
        }))
    }

    // No longer need these methods, as they're now integrated into parse_type_core
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
