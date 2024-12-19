use crate::{
    compilation::unit::CompilationUnit, context::GlobalContext,
    path::canonicalize_path_with_err_message,
};
use anstream::ColorChoice;
use anyhow::Result;
use indexmap::IndexMap;
use std::path::PathBuf;

#[derive(Debug)]
pub struct ImportIdentifier {
    pub main_name: String,
    pub rest: Option<Vec<String>>,
}

impl ImportIdentifier {
    pub fn file_name(&self) -> String {
        match &self.rest {
            Some(rest) => rest.join(std::path::MAIN_SEPARATOR_STR) + ".brim",
            None => "lib.brim".to_string(),
        }
    }

    /// Checks if spec is a module identifier and parses it. Module identifier consists of a main name and a sub name separated by a `::`.
    ///
    /// "std::io" -> build/deps/std/io.roan
    ///
    /// "std::io::file" -> build/deps/std/io/file.roan
    ///
    /// "some_dep" -> build/deps/some_dep/lib.roan
    ///
    /// "std" -> build/deps/std/lib.roan
    pub fn parse_module_identifier(spec: &str) -> Option<ImportIdentifier> {
        if !spec.contains("::") {
            return None;
        }

        let parts: Vec<&str> = spec.split("::").collect();

        if parts.len() >= 2 {
            Some(ImportIdentifier {
                main_name: parts[0].to_string(),
                rest: Some(parts[1..].iter().map(|s| s.to_string()).collect()),
            })
        } else {
            None
        }
    }
}

pub fn remove_surrounding_quotes(s: &str) -> &str {
    if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
        &s[1..s.len() - 1]
    } else {
        s
    }
}

pub fn resolve_referrer(referrer: &mut CompilationUnit, spec: &str) -> anyhow::Result<PathBuf> {
    let referrer_path = referrer.source.path.clone();
    let dir = referrer_path
        .parent()
        .expect("Unit referer path has no parent");

    let spec = if cfg!(windows) {
        spec.replace("/", "\\")
    } else {
        spec.to_string()
    };
    let str_path = remove_surrounding_quotes(&spec);

    let spec_path = PathBuf::from(str_path);

    let path = if spec_path.is_absolute() {
        spec_path
    } else {
        dir.join(spec_path)
    };

    Ok(path)
}

#[derive(Debug)]
pub struct UnitLoader {
    pub units: IndexMap<String, CompilationUnit>,
    pub cwd: PathBuf,
}

impl UnitLoader {
    pub fn new(cwd: PathBuf) -> Self {
        Self {
            units: IndexMap::new(),
            cwd,
        }
    }

    pub fn resolve_path(&self, spec: &str, unit: &mut CompilationUnit) -> Result<PathBuf> {
        let resolved_path = if let Some(ident) = ImportIdentifier::parse_module_identifier(spec) {
            let project_cwd = self
                .cwd
                .join("build")
                .join("deps")
                .join(ident.main_name.clone());

            let global = GlobalContext::from_cwd(project_cwd, ColorChoice::Auto)?;

            let parent = global.get_main_dir()?;
            canonicalize_path_with_err_message(parent.join(ident.file_name()), spec)?
        } else {
            canonicalize_path_with_err_message(resolve_referrer(unit, spec)?, spec)?
        };

        Ok(resolved_path)
    }

    pub fn load_unit(
        &mut self,
        spec: &str,
        referer: &mut CompilationUnit,
    ) -> Result<(String, CompilationUnit)> {
        let cache_key = remove_surrounding_quotes(spec).to_string();
        if let Some(unit) = self.units.get(&cache_key) {
            return Ok((cache_key, unit.clone()));
        }

        let cache_key = self
            .resolve_path(&cache_key, referer)?
            .to_string_lossy()
            .to_string();

        let unit = CompilationUnit::new(cache_key.clone().into())?;
        self.units.insert(cache_key.clone(), unit);

        Ok((
            cache_key.clone(),
            self.units.get(&cache_key).unwrap().clone(),
        ))
    }
}
