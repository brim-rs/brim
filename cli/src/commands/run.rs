use crate::{
    cli::{
        codegen_debug, debug_mode, dynamic_lib_mode, min_size_rel_mode, no_write,
        rel_with_deb_info_mode, release_mode, static_lib_mode,
    },
    graph::ProjectResolver,
    plural::plural,
};
use anstream::ColorChoice;
use anyhow::{Result, bail};
use brim::{
    CompiledModule, CompiledModules, ModuleId, Shell,
    args::RunArgs,
    compiler::CompilerContext,
    discover::ModuleDiscover,
    files::get_path,
    lints::Lints,
    resolver::ImportResolver,
    session::Session,
    temp_diag::TemporaryDiagnosticContext,
    toml::{Config, ProjectType},
    transformer::HirModuleMap,
};
use brim_ctx::errors::NoMainFunction;
use brim_parser::parser::Parser;
use clap::Command;
use std::{collections::HashSet, env::current_dir};

pub fn run_cmd() -> Command {
    Command::new("run")
        .about("Run a project")
        .arg(release_mode())
        .arg(debug_mode())
        .arg(min_size_rel_mode())
        .arg(rel_with_deb_info_mode())
        .arg(dynamic_lib_mode())
        .arg(static_lib_mode())
        .trailing_var_arg(true)
        .arg(
            clap::Arg::new("args")
                .num_args(0..)
                .allow_negative_numbers(true)
                .trailing_var_arg(true),
        )
        .arg(no_write())
        .arg(codegen_debug())
}

pub fn run_command(c_choice: ColorChoice, args: RunArgs, config: Config) -> Result<()> {
    let main_sess = &mut Session::new(current_dir()?, config.clone(), c_choice);
    let lints = Box::new(Lints::configure(&config.lints));
    let lints = Box::leak(lints);
    let main_ctx = &mut CompilerContext::new(args.clone(), lints);

    main_sess.assert_type(
        ProjectType::Bin,
        "Can only use `run` command on binary projects",
    )?;

    let mut resolver = ProjectResolver::new(".");
    let order = resolver.resolve_project()?;
    let configs = resolver.get_configs(&order);

    let compiled_projects = &mut CompiledModules::new();

    for config in configs {
        let sess = &mut Session::new(config.cwd.clone(), config.clone(), c_choice);
        let ctx = &mut CompilerContext::new(args.clone(), lints);
        let hir = compile_project(sess, ctx, c_choice, args.clone(), compiled_projects)?;

        compiled_projects
            .map
            .insert(config.project.name.clone(), CompiledModule {
                config,
                hir: hir.clone(),
            });
        compiled_projects.items.extend(hir.items);
    }

    // let code = comp.run_codegen(hir);
    // let file = build_dir.join("codegen").join("main.cpp");
    //
    // create_file_parent_dirs(&file)?;
    //
    // if !args.no_write && !file.exists() {
    //     bail!(
    //         "Found `no-write` flag but file doesn't exist. Try to run without `no-write` flag first"
    //     );
    // }
    //
    // if !args.no_write {
    //     std::fs::write(&file, code)?;
    // }
    //
    // let build_process =
    //     &mut CppBuild::new(Some(CompilerKind::Clang), project_type, build_dir, lib_type)?;
    // build_process.set_opt_level(opt_level).disable_warnings();
    // build_process.add_source(file);
    //
    // let exe_path = build_process.compile(project_name, shell)?;
    //
    // let args: Vec<String> = args.exec_args;
    //
    // let mut command = process::Command::new(&exe_path);
    // command.args(&args);
    //
    // shell.status(
    //     "Running",
    //     format!(
    //         "`{}{}{}`",
    //         &exe_path.to_string_lossy(),
    //         if args.is_empty() { "" } else { " " },
    //         &args.join(" ")
    //     ),
    // )?;
    //
    // command.status()?;

    Ok(())
}

pub fn bail_on_errors(len: usize) -> anyhow::Result<()> {
    if len > 0 {
        bail!(
            "Compilation failed due to {} previous {}",
            len,
            plural(len, "error", "errors")
        )
    }

    Ok(())
}

pub fn compile_project(
    sess: &mut Session,
    comp: &mut CompilerContext,
    c_choice: ColorChoice,
    args: RunArgs,
    compiled: &mut CompiledModules,
) -> Result<HirModuleMap> {
    let build_dir = sess.build_dir().clone();
    let project_type = sess.project_type();
    let lib_type = sess.lib_type();
    let project_name = sess.config.project.name.clone();
    let shell = &mut Shell::new(c_choice);
    let opt_level = sess.config.build.level.clone();

    let main_file = sess.main_file()?;

    shell.status(
        "Compiling",
        format!("{} in {} mode", project_name, opt_level),
    )?;

    let entry_file = sess.main_file()?;
    let mut parser = Parser::new(entry_file, sess.config.experimental.clone());
    let mut barrel = parser.parse_barrel()?;
    for diag in &parser.dcx.diags {
        comp.emit_diag(diag.clone());
    }

    let resolver_temp = &mut TemporaryDiagnosticContext::new();

    let mut discover = ModuleDiscover::new(resolver_temp, sess);

    let id = barrel.file_id.clone();
    discover
        .map
        .insert_or_update(get_path(entry_file)?, barrel.clone());
    let mut visited = HashSet::new();

    let module_map = discover.create_module_map(&mut barrel, id, &mut visited)?;
    let mut resolver = ImportResolver::new(resolver_temp, sess, compiled.clone(), module_map);
    let map = resolver.resolve()?;

    comp.extend_temp(resolver_temp.clone());
    bail_on_errors(comp.emitted.len())?;

    let hir = comp.analyze(map, compiled)?;

    if sess.config.is_bin() {
        let main_mod = hir.get_module(ModuleId::from_usize(entry_file)).unwrap();
        let main_fn = hir.get_fn(ModuleId::from_usize(entry_file), "main");

        if let Some(func) = main_fn {
            comp.validate_main_function(func, entry_file);
        } else {
            comp.emit(NoMainFunction {
                file: main_mod.path.display().to_string(),
            });
        }
    }

    bail_on_errors(comp.emitted.len())?;

    Ok(hir)
}
