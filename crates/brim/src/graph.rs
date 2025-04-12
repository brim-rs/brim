use anyhow::Result;
use brim_config::toml::Config;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    path::PathBuf,
};

pub struct ProjectResolver {
    base_path: PathBuf,
    loaded_configs: HashMap<String, Config>,
}

impl ProjectResolver {
    pub fn new(base_path: impl Into<PathBuf>) -> Self {
        Self { base_path: base_path.into(), loaded_configs: HashMap::new() }
    }

    pub fn resolve_project(&mut self) -> Result<Vec<String>> {
        let current_config = Config::get(&self.base_path, None)?;
        let project_name = current_config.project.name.clone();

        self.loaded_configs.insert(project_name.clone(), current_config);

        let mut to_process = VecDeque::new();
        to_process.push_back(project_name.clone());

        while let Some(project_name) = to_process.pop_front() {
            let config = self
                .loaded_configs
                .get(&project_name)
                .ok_or_else(|| anyhow::anyhow!("Config not found for {}", project_name))?;

            for (dep_name, dep_info) in &config.dependencies.clone() {
                if !self.loaded_configs.contains_key(dep_name) {
                    to_process.push_back(dep_name.clone());

                    let dep_path = if let Some(path) = &dep_info.path {
                        self.base_path.join(path)
                    } else if let Some(_github) = &dep_info.github {
                        return Err(anyhow::anyhow!("GitHub dependencies not yet implemented"));
                    } else {
                        return Err(anyhow::anyhow!(
                            "Invalid dependency configuration for {}",
                            dep_name
                        ));
                    };

                    let dep_config = Config::get(&dep_path, None)?;
                    self.loaded_configs.insert(dep_name.to_string(), dep_config);
                }
            }
        }

        let order = resolve_build_order(&self.loaded_configs, &project_name)?;

        Ok(order)
    }

    pub fn get_configs(&self, order: &[String]) -> Vec<Config> {
        order.iter().filter_map(|name| self.loaded_configs.get(name)).cloned().collect()
    }
}

fn resolve_build_order(
    configs: &HashMap<String, Config>,
    start_project: &str,
) -> Result<Vec<String>> {
    let mut resolved = Vec::new();
    let mut visited = HashSet::new();
    let mut temp_marks = HashSet::new();

    fn visit(
        project: &str,
        configs: &HashMap<String, Config>,
        resolved: &mut Vec<String>,
        visited: &mut HashSet<String>,
        temp_marks: &mut HashSet<String>,
    ) -> Result<()> {
        if temp_marks.contains(project) {
            return Err(anyhow::anyhow!("Cyclic dependency detected: {}", project));
        }

        if visited.contains(project) {
            return Ok(());
        }

        let config = configs
            .get(project)
            .ok_or_else(|| anyhow::anyhow!("Missing dependency: {}", project))?;

        temp_marks.insert(project.to_string());

        for dep_name in config.dependencies.keys() {
            visit(dep_name, configs, resolved, visited, temp_marks)?;
        }

        temp_marks.remove(project);
        visited.insert(project.to_string());
        resolved.push(project.to_string());

        Ok(())
    }

    visit(start_project, configs, &mut resolved, &mut visited, &mut temp_marks)?;

    Ok(resolved)
}
