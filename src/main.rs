//extern crate inflector;
extern crate nom;

mod parser;

use std::io::{self, Read};
use std::env;
use std::process::exit;
use self::parser::*;

fn main() {

  let args: Vec<String> = env::args().collect();
  type F = fn(&String) -> String;

  let select = | conversion : &String | -> Option<F> {

    match conversion.as_ref() {
        "upper"          => Some(|s| s.to_uppercase()),
        "lower"          => Some(|s| s.to_lowercase()),
        "camel"          => Some(|s| to_case(s, CaseType::Camel)),
        "kebab"          => Some(|s| to_case(s, CaseType::Kebab)),
        "train"          => Some(|s| to_case(s, CaseType::Train)),
        "sentence"       => Some(|s| to_case(s, CaseType::Sentence)),
        "snake"          => Some(|s| to_case(s, CaseType::Snake)),
        "pascal"         => Some(|s| to_case(s, CaseType::Pascal)),
        "screamingsnake" => Some(|s| to_case(s, CaseType::Screamingsnake)),
        "title"          => Some(|s| to_case(s, CaseType::Title)),
        _                => None,
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
           print!("{}", f(&line));
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
        print!("{}", f(a)),
      None =>
        exit(2)
    };
  }
  else {
        exit(1)
  }


}
