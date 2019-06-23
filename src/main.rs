extern crate inflector;

use std::io::{self, Read};
use std::env;
use inflector::Inflector;

fn main() {

  let args: Vec<String> = env::args().collect();
  type F = fn(&String) -> String;

  let select = | conversion : &String | -> Option<F> {

    match conversion.as_ref() {
        "upper" => Some(|s| s.to_uppercase()),
        "lower" => Some(|s| s.to_lowercase()),
        "camel" => Some(|s| s.to_camel_case()),
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
        println!("{}", "Uuups")
    };

  }
  else if args.len() == 3 {
    let a = &args[2];
    match select(&args[1]) {
      Some(f) =>
        println!("{}", f(a)),
      None =>
        println!("{}", "Uuups")
    };
  }
  else {
    println!("{}", "ups");
  }


}
