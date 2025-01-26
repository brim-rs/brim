use crate::codegen::CppCodegen;
use brim_ast::ty::PrimitiveType;
use brim_hir::ty::HirTyKind;

impl CppCodegen {
    pub fn transform_ty(&mut self, ty: HirTyKind) -> String {
        match ty {
            HirTyKind::Primitive(prim) => self.transform_primitive(prim),
            HirTyKind::Ptr(ty, cnst) => {
                let ty = self.transform_ty(ty.kind);
                if cnst.as_bool() {
                    format!("const {}*", ty)
                } else {
                    format!("{}*", ty)
                }
            }

            HirTyKind::Ref(ty, cnst) => {
                let ty = self.transform_ty(ty.kind);
                if cnst.as_bool() {
                    format!("const {}&", ty)
                } else {
                    format!("{}&", ty)
                }
            }

            HirTyKind::Ident { ident, generics } => {
                format!("{}{}", ident, if generics.params.is_empty() {
                    "".to_string()
                } else {
                    format!("<{}>", generics.params.iter().map(|p| self.transform_ty(p.ty.kind.clone())).collect::<Vec<_>>().join(", "))
                })
            }

            HirTyKind::Vec(ty) => {
                let ty = self.transform_ty(ty.kind);
                format!("std::vector<{}>", ty)
            }

            _ => todo!("transform_ty: {:?}", ty),
        }
    }

    fn transform_primitive(&self, prim: PrimitiveType) -> String {
        match prim {
            PrimitiveType::Void => "void".to_string(),
            PrimitiveType::I8 => "int8_t".to_string(),
            PrimitiveType::I16 => "int16_t".to_string(),
            PrimitiveType::I32 => "int32_t".to_string(),
            PrimitiveType::I64 => "int64_t".to_string(),

            PrimitiveType::U8 => "uint8_t".to_string(),
            PrimitiveType::U16 => "uint16_t".to_string(),
            PrimitiveType::U32 => "uint32_t".to_string(),
            PrimitiveType::U64 => "uint64_t".to_string(),

            PrimitiveType::F32 => "float".to_string(),
            PrimitiveType::F64 => "double".to_string(),

            PrimitiveType::Bool => "bool".to_string(),

            PrimitiveType::Char => "char".to_string(),
            PrimitiveType::Str => "std::string".to_string(),
        }
    }
}
