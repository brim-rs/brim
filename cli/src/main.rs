use std::path::PathBuf;
use brim::session::Session;

fn main() {
    let mut sess = Session::default();
    let main_path= PathBuf::from("main.brim");

    sess.add_file(main_path.clone(), "fn main() {}".to_string());
    println!("{:?}", sess);

    sess.add_file(main_path.clone(), "fn mai) {}".to_string());
    println!("{:?}", sess);

    let file = sess.get_file_by_name(&main_path).unwrap();
    println!("{:?}", file);
}
