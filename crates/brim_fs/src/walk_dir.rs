use std::path::PathBuf;

pub fn walk_for_file(mut dir: PathBuf, file_name: &str) -> Option<PathBuf> {
    loop {
        let file_path = dir.join(file_name);
        if file_path.exists() {
            return Some(file_path);
        }

        if !dir.pop() {
            break;
        }
    }

    None
}
