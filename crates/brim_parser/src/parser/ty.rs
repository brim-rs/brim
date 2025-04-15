use crate::{
    parser::{PResult, PToken, PTokenKind, Parser},
    ptok,
};
use brim_ast::{
    Const, Mut, Type,
    expr::Expr,
    item::{Block, Ident, ItemKind, TypeAlias, TypeAliasValue},
    stmts::{Stmt, StmtKind},
    token::{BinOpToken, Delimiter, Orientation, TokenKind},
    ty::{Mutable, PrimitiveType, Ty, TyKind},
};
use brim_span::span::Span;

impl Parser {
    pub fn parse_type(&mut self) -> PResult<Ty> {
        let mut ty = self.parse_type_core()?;

        ty = self.apply_type_modifiers(ty)?;

        Ok(ty)
    }

    fn parse_type_core(&mut self) -> PResult<Ty> {
        let span = self.current().span;

        let kind = if self.eat(TokenKind::BinOp(BinOpToken::And)) {
            let mutable = self.is_mut();
            let inner_ty = self.parse_type_core()?;
            TyKind::Ref(span, Box::new(inner_ty), mutable)
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            let mutable = self.is_mut();
            let inner_ty = self.parse_type_core()?;
            TyKind::Ptr(span, Box::new(inner_ty), mutable)
        } else if self.current().is_keyword(Mut) {
            let span = self.current().span;
            self.eat_keyword(ptok!(Mut));
            let inner_ty = self.parse_type_core()?;
            TyKind::Mut(Box::new(inner_ty), span)
        } else if self.current().is_keyword(Const) {
            let span = self.current().span;
            self.eat_keyword(ptok!(Const));
            let inner_ty = self.parse_type_core()?;
            TyKind::Const(span, Box::new(inner_ty))
        } else {
            let ident = self.parse_ident()?;

            if let Some(primitive) = self.is_primitive(ident)? {
                TyKind::Primitive(primitive)
            } else {
                TyKind::Ident { ident, generics: self.parse_argument_generics()? }
            }
        };

        Ok(Ty { span, kind, id: self.new_id() })
    }

    fn apply_type_modifiers(&mut self, mut ty: Ty) -> PResult<Ty> {
        let span = ty.span;

        // Handle array/vector types
        if self.current().is_delimiter(Delimiter::Bracket, Orientation::Open)
            && self.next().is_delimiter(Delimiter::Bracket, Orientation::Close)
        {
            self.expect_obracket()?;
            self.expect_cbracket()?;

            // Apply the vector modifier with consideration for type modifiers
            match ty.kind {
                // For const types, we want const(T[]) not (const T)[]
                TyKind::Const(span, inner_ty) => {
                    let element_ty = *inner_ty;
                    ty = Ty {
                        span,
                        kind: TyKind::Const(
                            span,
                            Box::new(Ty {
                                span: element_ty.span,
                                kind: TyKind::Vec(Box::new(element_ty)),
                                id: self.new_id(),
                            }),
                        ),
                        id: self.new_id(),
                    };
                }
                // For mut types, we want mut(T[]) not (mut T)[]
                TyKind::Mut(inner_ty, span) => {
                    let element_ty = *inner_ty;
                    ty = Ty {
                        span,
                        kind: TyKind::Mut(
                            Box::new(Ty {
                                span: element_ty.span,
                                kind: TyKind::Vec(Box::new(element_ty)),
                                id: self.new_id(),
                            }),
                            span,
                        ),
                        id: self.new_id(),
                    };
                }
                // For ref types, apply vec to the inner type: &(T[]) not (&T)[]
                TyKind::Ref(span, inner_ty, mutability) => {
                    let element_ty = *inner_ty;
                    ty = Ty {
                        span,
                        kind: TyKind::Ref(
                            span,
                            Box::new(Ty {
                                span: element_ty.span,
                                kind: TyKind::Vec(Box::new(element_ty)),
                                id: self.new_id(),
                            }),
                            mutability,
                        ),
                        id: self.new_id(),
                    };
                }
                // For ptr types, apply vec to the inner type: *(T[]) not (*T)[]
                TyKind::Ptr(span, inner_ty, mutability) => {
                    let element_ty = *inner_ty;
                    ty = Ty {
                        span,
                        kind: TyKind::Ptr(
                            span,
                            Box::new(Ty {
                                span: element_ty.span,
                                kind: TyKind::Vec(Box::new(element_ty)),
                                id: self.new_id(),
                            }),
                            mutability,
                        ),
                        id: self.new_id(),
                    };
                }
                _ => {
                    ty = Ty { span, kind: TyKind::Vec(Box::new(ty)), id: self.new_id() };
                }
            }
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
            let mark = self.current().span;
            self.eat(TokenKind::QuestionMark);
            ty = Ty {
                span: span.to(self.prev().span),
                kind: TyKind::Option(Box::new(ty), mark),
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
            inner_ty_opt.map(|inner_ty| TyKind::Ref(span, Box::new(inner_ty), is_mut))
        } else if self.eat(TokenKind::BinOp(BinOpToken::Star)) {
            let is_mut = self.is_mut();
            let inner_ty_opt = self.parse_type_core_without_ident()?;
            inner_ty_opt.map(|inner_ty| TyKind::Ptr(span, Box::new(inner_ty), is_mut))
        } else if self.current().is_keyword(Mut) {
            let span = self.current().span;
            self.eat_keyword(ptok!(Mut));
            let inner_ty_opt = self.parse_type_core_without_ident()?;
            inner_ty_opt.map(|inner_ty| TyKind::Mut(Box::new(inner_ty), span))
        } else {
            let ident_opt = self.parse_ident_without_err()?;

            if let Some(ident) = ident_opt {
                self.is_primitive(ident)?.map(TyKind::Primitive)
            } else {
                None
            }
        };

        Ok(kind_opt.map(|kind| Ty { span, kind, id: self.new_id() }))
    }

    // No longer need these methods, as they're now integrated into parse_type_core
    pub fn parse_mut(&mut self) -> PResult<TyKind> {
        let span = self.current().span;
        self.eat_keyword(ptok!(Mut));
        Ok(TyKind::Mut(Box::new(self.parse_type()?), span))
    }

    pub fn is_mut(&mut self) -> Mutable {
        if self.current().is_keyword(Mut) {
            self.eat_keyword(ptok!(Mut));

            Mutable::from_bool(true, self.prev().span)
        } else {
            Mutable::from_bool(false, Span::DUMMY)
        }
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
            ItemKind::TypeAlias(TypeAlias { span: span.to(self.prev().span), generics, ident, ty }),
        ))
    }
}
