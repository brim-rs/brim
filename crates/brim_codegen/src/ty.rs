use crate::codegen::CppCodegen;
use brim_ast::ty::PrimitiveType;
use brim_hir::ty::HirTyKind;

impl CppCodegen {
    pub fn generate_ty(&mut self, ty: HirTyKind) -> String {
        match ty {
            HirTyKind::Primitive(prim) => self.transform_primitive(prim),
            HirTyKind::Ptr(ty, cnst) => {
                let ty = self.generate_ty(*ty);
                if cnst.as_bool() {
                    format!("const {}*", ty)
                } else {
                    format!("{}*", ty)
                }
            }

            HirTyKind::Ref(ty, cnst) => {
                let ty = self.generate_ty(*ty);
                if cnst.as_bool() {
                    format!("const {}&", ty)
                } else {
                    format!("{}&", ty)
                }
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
                        .unwrap();

                    let mod_id = symbol.id.mod_id;

                    format!("module{}::{}{}", mod_id.as_usize(), ident.name, generics)
                }
            }

            HirTyKind::Vec(ty) => {
                let ty = self.generate_ty(*ty);
                format!("std::vector<{}>", ty)
            }

            HirTyKind::Array(ty, size) => {
                let ty = self.generate_ty(*ty);
                format!("std::array<{}, {}>", ty, size)
            }

            HirTyKind::Const(ty) => {
                let ty = self.generate_ty(*ty);
                format!("const {}", ty)
            }

            // Only for now, this will be replaced in type checking
            HirTyKind::Placeholder | HirTyKind::Err(_) => "auto".to_string(),

            _ => todo!("transform_ty: {:?}", ty),
        }
    }

    fn transform_primitive(&self, prim: PrimitiveType) -> String {
        match prim {
            PrimitiveType::Void => "void".to_string(),
            PrimitiveType::I8 => "std::int8_t".to_string(),
            PrimitiveType::I16 => "std::int16_t".to_string(),
            PrimitiveType::I32 => "std::int32_t".to_string(),
            PrimitiveType::I64 => "std::int64_t".to_string(),

            PrimitiveType::U8 => "std::uint8_t".to_string(),
            PrimitiveType::U16 => "std::uint16_t".to_string(),
            PrimitiveType::U32 => "std::uint32_t".to_string(),
            PrimitiveType::U64 => "std::uint64_t".to_string(),

            PrimitiveType::F32 => "float".to_string(),
            PrimitiveType::F64 => "double".to_string(),

            PrimitiveType::Bool => "bool".to_string(),

            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Str => "std::string".to_string(),

            PrimitiveType::Any => "SafeAny".to_string(),
        }
    }
}
