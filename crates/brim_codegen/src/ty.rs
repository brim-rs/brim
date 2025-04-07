use crate::codegen::CppCodegen;
use brim_ast::ty::PrimitiveType;
use brim_hir::ty::HirTyKind;

impl CppCodegen {
    pub fn generate_ty(&mut self, ty: HirTyKind) -> String {
        match ty {
            HirTyKind::Primitive(prim) => self.transform_primitive(prim),
            HirTyKind::Ptr(ty, mutable) => {
                let ty = self.generate_ty(*ty);
                format!("{}*", ty)
            }

            HirTyKind::Ref(ty, mutable) => {
                let ty = self.generate_ty(*ty);
                format!("{}&", ty)
            }

            HirTyKind::Ident {
                ident,
                generics,
                is_generic,
            } => {
                let generics = if generics.params.is_empty() {
                    "".to_string()
                } else {
                    format!(
                        "<{}>",
                        generics
                            .params
                            .iter()
                            .map(|p| self.generate_ty(p.ty.clone()))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                };

                if is_generic {
                    format!("{}{}", ident.name, generics)
                } else {
                    let symbol = self
                        .hir()
                        .symbols
                        .resolve(&ident.to_string(), self.current_mod.as_usize())
                        .expect(&format!(
                            "Failed to resolve symbol: {}",
                            ident.name.to_string()
                        ));

                    let mod_id = symbol.id.mod_id;

                    format!(
                        "module{}::brim_{}{}",
                        mod_id.as_usize(),
                        ident.name,
                        generics
                    )
                }
            }

            HirTyKind::Vec(ty) => {
                let ty = self.generate_ty(*ty);
                format!("std::vector<{}>", ty)
            }

            HirTyKind::Mut(ty) => {
                let ty = self.generate_ty(*ty);
                format!("{}", ty)
            }

            HirTyKind::Const(ty) => {
                let ty = self.generate_ty(*ty);
                format!("const {}", ty)
            }

            HirTyKind::Result(ok, err) => {
                let ok = self.generate_ty(*ok);
                let err = self.generate_ty(*err);
                format!("std::expected<{}, {}>", ok, err)
            }

            HirTyKind::ResultOk(ty) | HirTyKind::ResultErr(ty) => self.generate_ty(*ty),

            HirTyKind::Option(ty) => {
                let ty = self.generate_ty(*ty);
                format!("std::optional<{}>", ty)
            }

            HirTyKind::Some(ty) => {
                let ty = self.generate_ty(*ty);
                format!("std::optional<{}>", ty)
            }

            // Only for now, this will be replaced in type checking
            HirTyKind::Placeholder => "auto".to_string(),
            HirTyKind::Err(_) => panic!("some unhandled error"),

            _ => todo!("transform_ty: {:?}", ty),
        }
    }

    fn transform_primitive(&self, prim: PrimitiveType) -> String {
        match prim {
            PrimitiveType::Void => "void",
            PrimitiveType::I8 => "std::int8_t",
            PrimitiveType::I16 => "std::int16_t",
            PrimitiveType::I32 => "std::int32_t",
            PrimitiveType::I64 => "std::int64_t",
            PrimitiveType::Isize => "std::intptr_t",

            PrimitiveType::U8 => "std::uint8_t",
            PrimitiveType::U16 => "std::uint16_t",
            PrimitiveType::U32 => "std::uint32_t",
            PrimitiveType::U64 => "std::uint64_t",
            PrimitiveType::Usize => "std::uintptr_t",

            PrimitiveType::F32 => "float",
            PrimitiveType::F64 => "double",

            PrimitiveType::Bool => "bool",

            PrimitiveType::Char => "char",
            PrimitiveType::String => "std::string",

            PrimitiveType::Any => "std::any",
        }
        .to_string()
    }
}
