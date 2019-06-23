extern crate inflector;

use std::io::{self, Read};
use std::env;
use inflector::Inflector;
use std::process::exit;

fn main() {

  let args: Vec<String> = env::args().collect();
  type F = fn(&String) -> String;

  let select = | conversion : &String | -> Option<F> {

    match conversion.as_ref() {
        "upper" => Some(|s| s.to_uppercase()),
        "lower" => Some(|s| s.to_lowercase()),
        "camel" => Some(|s| s.to_camel_case()),
        "class" => Some(|s| s.to_class_case()),
        "kebab" => Some(|s| s.to_kebab_case()),
        "train" => Some(|s| s.to_train_case()),
        "table" => Some(|s| s.to_table_case()),
        "sentence" => Some(|s| s.to_sentence_case()),
        "snake" => Some(|s| s.to_snake_case()),
        "pascal" => Some(|s| s.to_pascal_case()),
        "screamingsnake" => Some(|s| s.to_screaming_snake_case()),
        "title" => Some(|s| s.to_title_case()),
        _ => None,
    }

  };

  if args.len() == 2 {

    match select(&args[1]) {
      Some(f) => {
        let stdin = io::stdin();
        let mut stdin = stdin.lock(); // locking is optional

        let mut line = String::new();

        // Could also `match` on the `Result` if you wanted to handle `Err` 
        while let Ok(n_bytes) = stdin.read_to_string(&mut line) {
           if n_bytes == 0 { break }
           println!("{}", f(&line));
           line.clear();
        }
      },
      None =>
        exit(2)
    };

  }
  else if args.len() == 3 {
    let a = &args[2];
    match select(&args[1]) {
      Some(f) =>
        println!("{}", f(a)),
      None =>
        exit(2)
    };
  }
  else {
        exit(1)
  }


}
