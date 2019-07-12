
use nom::{
  IResult,
  bytes::complete::*,
  combinator::*,
  multi::*,
  character::complete::char,
  branch::alt,
};

fn to_lower(s: &str) -> String {
  format!("{}", s).to_lowercase()
}

fn is_alphabetic(c: char) -> bool {
  c.is_alphabetic()
}

fn is_lowercase(c: char) -> bool {
  c.is_lowercase()
}

fn is_uppercase(c: char) -> bool {
  c.is_uppercase()
}

fn is_lowercont(c: char) -> bool {
  c.is_lowercase() || c.is_digit(10)
}

fn is_uppercont(c: char) -> bool {
  c.is_uppercase() || c.is_digit(10)
}

fn is_alphabeticcont(c: char) -> bool {
  c.is_alphabetic() || c.is_digit(10)
}

fn lowercont_segment(input: &str) -> IResult<&str, String> {
  map(
    take_while1(is_lowercont),
    to_lower
  )(input)
}

fn uppercont_segment(input: &str) -> IResult<&str, String> {
  map(
    take_while1(is_uppercont),
    to_lower
  )(input)
}

fn anycont_segment(input: &str) -> IResult<&str, String> {
  map(
    take_while1(is_alphabeticcont),
    to_lower
  )(input)
}

fn lower_single(input: &str) -> IResult<&str, String> {
  map(
    take_while_m_n(1, 1, is_lowercase),
    to_lower
  )(input)
}

fn upper_single(input: &str) -> IResult<&str, String> {
  map(
    take_while_m_n(1, 1, is_uppercase),
    to_lower
  )(input)
}

fn any_single(input: &str) -> IResult<&str, String> {
  map(
    take_while_m_n(1, 1, is_alphabetic),
    to_lower
  )(input)
}

fn lower_segment(input: &str) -> IResult<&str, String> {
  let (input, head) = lower_single(input)?;
  let (input, tail) = lowercont_segment(input)?;

  let r = format!("{}{}", head, tail);

  Ok((input, r))
}

fn upper_segment(input: &str) -> IResult<&str, String> {
  let (input, head) = upper_single(input)?;
  let (input, tail) = uppercont_segment(input)?;

  let r = format!("{}{}", head, tail);

  Ok((input, r))
}

fn title_segment(input: &str) -> IResult<&str, String> {
  let (input, head) = upper_single(input)?;
  let (input, tail) = lower_segment(input)?;

  let r = format!("{}{}", head, tail);

  Ok((input, r))
}

fn any_segment(input: &str) -> IResult<&str, String> {
  let (input, head) = any_single(input)?;
  let (input, tail) = anycont_segment(input)?;

  let r = format!("{}{}", head, tail);

  Ok((input, r))
}

pub fn prefixed_segment(
    prefix: char,
    segment_parser: impl Fn(&str) -> IResult<&str, String>,
) -> impl Fn(&str) -> IResult<&str, String> {
  move |input: &str| {
    let (input, _ ) = char(prefix)(input)?;
    let (input, r) = segment_parser(input)?;

    Ok((input, r))
  }
}

fn pascal_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = title_segment(input)?;
  let (input, mut tail) = many1(title_segment)(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn camel_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = lower_segment(input)?;
  let (input, mut tail) = many1(title_segment)(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn kebab_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = lower_segment(input)?;
  let (input, mut tail) = many1(prefixed_segment('-', lower_segment))(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn train_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = title_segment(input)?;
  let (input, mut tail) = many1(prefixed_segment('-', title_segment))(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn snake_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = lower_segment(input)?;
  let (input, mut tail) = many1(prefixed_segment('_', lower_segment))(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn screamingsnake_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = upper_segment(input)?;
  let (input, mut tail) = many1(prefixed_segment('_', upper_segment))(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn sentence_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = any_segment(input)?;
  let (input, mut tail) = many1(prefixed_segment(' ', any_segment))(input)?;

  let mut r = vec!(head);

  r.append(&mut tail);

  Ok((input, Token::Case(r)))
}

fn word_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = any_segment(input)?;

  let r = vec!(head);

  Ok((input, Token::Case(r)))
}

fn char_case(input: &str) -> IResult<&str, Token> {
  let (input, head) = any_single(input)?;

  let r = vec!(head);

  Ok((input, Token::Case(r)))
}

fn single_case(input: &str) -> IResult<&str, Token> {
  alt((kebab_case, snake_case, screamingsnake_case, train_case, camel_case, pascal_case, sentence_case, word_case, char_case))(input)
}

fn text(input: &str) -> IResult<&str, Token> {
    let (input, s) = take_till1(is_alphabetic)(input)?;
    let r = Token::Text(s.to_string());

    Ok((input, r))
}

fn token(input: &str) -> IResult<&str, Token> {
  alt((text, single_case))(input)
}

fn parse(input: &str) -> IResult<&str, Vec<Token>> {
  many0(token)(input)
}

#[derive(Debug,PartialEq)]
enum Token {
  Case(Vec<String>),
  Text(String),

// "camel" =>          oneTwoThree
// "pascal" =>         OneTwoThree
// "kebab" =>          one-two-three
// "train" =>          One-Two-Three
// "snake" =>          one_two_three
// "screamingsnake" => ONE_TWO_THREE
// "sentence" =>       One two three
}

pub enum CaseType {
  Upper,
  Lower,
  Camel,
  Kebab,
  Train,
  Snake,
  Pascal,
  Screamingsnake,

  Sentence,
  Title,
}

fn to_title_case(s: &String) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn render_token(t: &Token, case_type: &CaseType) -> String {
  match t {
    Token::Text(s) => format!("{}", s),
    Token::Case(vs_orig) => {
      let mut vs = vs_orig.clone();
      match case_type {
        CaseType::Upper =>
          vs.iter().map(|s| s.to_uppercase()).collect::<Vec<_>>().join(""),
        CaseType::Lower =>
          vs.join(""),
        CaseType::Snake =>
          vs.join("_"),
        CaseType::Kebab =>
          vs.join("-"),
        CaseType::Screamingsnake =>
          vs.iter().map(|s| s.to_uppercase()).collect::<Vec<_>>().join("_"),
        CaseType::Pascal =>
          vs.iter().map(to_title_case).collect::<Vec<_>>().join(""),
        CaseType::Train =>
          vs.iter().map(to_title_case).collect::<Vec<_>>().join("-"),
        CaseType::Title =>
          vs.iter().map(to_title_case).collect::<Vec<_>>().join(" "),
        CaseType::Sentence => {
          vs[0] = to_title_case(& vs[0]);
          vs.join(" ")},

        CaseType::Camel => {
          let mut vs2 =
            vs.iter().map(to_title_case).collect::<Vec<_>>();
          vs2[0] = vs[0].to_string();
          vs2.join("")},
      }
    }
  }
}

fn render(tokens: Vec<Token>, case_type: CaseType) -> String {
  tokens.iter().map( |t| render_token(t, &case_type)).collect::<Vec<_>>().join("")
}

pub fn to_case(input: &str, case_type: CaseType) -> String {
  let tokens = parse(input).unwrap().1;
  render(tokens, case_type)
}

#[test]
fn parse_cases() {

  assert_eq!(lower_segment("abcd"), Ok(("", format!("{}", "abcd"))));
  assert_ne!(lower_segment("Abcd"), Ok(("", format!("{}", "abcd"))));

  assert_eq!(upper_segment("ABCD"), Ok(("", format!("{}", "abcd"))));
  assert_ne!(upper_segment("Abcd"), Ok(("", format!("{}", "abcd"))));

  assert_eq!(title_segment("Abcd"), Ok(("", format!("{}", "abcd"))));
  assert_ne!(title_segment("ABCD"), Ok(("", format!("{}", "abcd"))));
  assert_ne!(title_segment("abcd"), Ok(("", format!("{}", "abcd"))));

  assert_eq!(pascal_case("OneTwoThree"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_ne!(pascal_case("oneTwoThree"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));

  assert_eq!(camel_case("oneTwoThree"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(kebab_case("one-two-three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(train_case("One-Two-Three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(snake_case("one_two_three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(screamingsnake_case("ONE_TWO_THREE"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));

  assert_eq!(sentence_case("ONE TWO THREE"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(sentence_case("one two three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(sentence_case("One Two Three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(sentence_case("oNe tWo thRee"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));


  assert_eq!(single_case("OneTwoThree"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));

  assert_eq!(single_case("oneTwoThree"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("one-two-three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("One-Two-Three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("one_two_three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("ONE_TWO_THREE"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));

  assert_eq!(single_case("ONE TWO THREE"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("one two three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("One Two Three"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));
  assert_eq!(single_case("oNe tWo thRee"), Ok(("", Token::Case(vec!("one", "two", "three").iter().map(|&s| s.to_string()).collect::<Vec<_>>()))));

  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Upper), "ONETWOTHREE".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Lower), "onetwothree".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Snake), "one_two_three".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Kebab), "one-two-three".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Screamingsnake), "ONE_TWO_THREE".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Pascal), "OneTwoThree".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Train), "One-Two-Three".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Title), "One Two Three".to_string());
  assert_eq!(render_token(&single_case("oneTwoThree").unwrap().1, &CaseType::Sentence), "One two three".to_string());
  assert_eq!(render_token(&single_case("one_two_three").unwrap().1, &CaseType::Camel), "oneTwoThree".to_string());

  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Upper), "ONETWOTHREE".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Lower), "onetwothree".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Snake), "one_two_three".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Kebab), "one-two-three".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Screamingsnake), "ONE_TWO_THREE".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Pascal), "OneTwoThree".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Train), "One-Two-Three".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Title), "One Two Three".to_string());
  assert_eq!(render(parse("oneTwoThree").unwrap().1, CaseType::Sentence), "One two three".to_string());
  assert_eq!(render(parse("one_two_three").unwrap().1, CaseType::Camel), "oneTwoThree".to_string());

  assert_eq!(to_case("oneTwoThree", CaseType::Upper), "ONETWOTHREE".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Lower), "onetwothree".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Snake), "one_two_three".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Kebab), "one-two-three".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Screamingsnake), "ONE_TWO_THREE".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Pascal), "OneTwoThree".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Train), "One-Two-Three".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Title), "One Two Three".to_string());
  assert_eq!(to_case("oneTwoThree", CaseType::Sentence), "One two three".to_string());
  assert_eq!(to_case("one_two_three", CaseType::Camel), "oneTwoThree".to_string());

  assert_eq!(to_case("|oneTwoThree", CaseType::Upper), "|ONETWOTHREE".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Lower), "|onetwothree".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Snake), "|one_two_three".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Kebab), "|one-two-three".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Screamingsnake), "|ONE_TWO_THREE".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Pascal), "|OneTwoThree".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Train), "|One-Two-Three".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Title), "|One Two Three".to_string());
  assert_eq!(to_case("|oneTwoThree", CaseType::Sentence), "|One two three".to_string());
  assert_eq!(to_case("|one_two_three", CaseType::Camel), "|oneTwoThree".to_string());

  assert_eq!(to_case("|oneTwoThree|", CaseType::Upper), "|ONETWOTHREE|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Lower), "|onetwothree|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Snake), "|one_two_three|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Kebab), "|one-two-three|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Screamingsnake), "|ONE_TWO_THREE|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Pascal), "|OneTwoThree|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Train), "|One-Two-Three|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Title), "|One Two Three|".to_string());
  assert_eq!(to_case("|oneTwoThree|", CaseType::Sentence), "|One two three|".to_string());
  assert_eq!(to_case("|one_two_three|", CaseType::Camel), "|oneTwoThree|".to_string());

  assert_eq!(to_case("oneTwoThree|", CaseType::Upper), "ONETWOTHREE|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Lower), "onetwothree|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Snake), "one_two_three|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Kebab), "one-two-three|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Screamingsnake), "ONE_TWO_THREE|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Pascal), "OneTwoThree|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Train), "One-Two-Three|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Title), "One Two Three|".to_string());
  assert_eq!(to_case("oneTwoThree|", CaseType::Sentence), "One two three|".to_string());
  assert_eq!(to_case("one_two_three|", CaseType::Camel), "oneTwoThree|".to_string());

  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Upper), "ONETWOTHREE|ONETWOTHREE".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Lower), "onetwothree|onetwothree".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Snake), "one_two_three|one_two_three".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Kebab), "one-two-three|one-two-three".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Screamingsnake), "ONE_TWO_THREE|ONE_TWO_THREE".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Pascal), "OneTwoThree|OneTwoThree".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Train), "One-Two-Three|One-Two-Three".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Title), "One Two Three|One Two Three".to_string());
  assert_eq!(to_case("oneTwoThree|oneTwoThree", CaseType::Sentence), "One two three|One two three".to_string());
  assert_eq!(to_case("one_two_three|one_two_three", CaseType::Camel), "oneTwoThree|oneTwoThree".to_string());

  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Upper), "ONETWOTHREE ONETWOTHREE".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Lower), "onetwothree onetwothree".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Snake), "one_two_three one_two_three".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Kebab), "one-two-three one-two-three".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Screamingsnake), "ONE_TWO_THREE ONE_TWO_THREE".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Pascal), "OneTwoThree OneTwoThree".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Train), "One-Two-Three One-Two-Three".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Title), "One Two Three One Two Three".to_string());
  assert_eq!(to_case("oneTwoThree oneTwoThree", CaseType::Sentence), "One two three One two three".to_string());
  assert_eq!(to_case("one_two_three one_two_three", CaseType::Camel), "oneTwoThree oneTwoThree".to_string());

  assert_eq!(to_case("one two three|", CaseType::Camel), "oneTwoThree|".to_string());

  assert_eq!(to_case("", CaseType::Camel), "".to_string());
  assert_eq!(to_case("s", CaseType::Camel), "s".to_string());
  assert_eq!(to_case("          \"camel\"          => Some(|s| to_case(s, CaseType::Camel)),", CaseType::Camel), "          \"camel\"          => some(|s| toCase(s, caseType::camel)),".to_string());
}

