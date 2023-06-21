#![allow(dead_code)]

mod string;
use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while},
    character::complete::{alphanumeric1, char, digit1, one_of},
    combinator::{cut, map, opt, value},
    error::{context, ContextError, FromExternalError, ParseError},
    multi::separated_list0,
    number::complete::{double, float},
    sequence::{delimited, preceded, separated_pair, terminated},
    AsChar, Err, IResult, InputTakeAtPosition, Parser,
};

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
#[serde(untagged)]
enum DataModel<'a> {
    Null,                                 // ✅
    Boolean(bool),                        // ✅
    Float(f64),                           // ✅
    String(String),                       // ✅
    Map(HashMap<&'a str, DataModel<'a>>), // ✅
    Vec(Vec<DataModel<'a>>),              // ✅
}

fn spacer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";

    take_while(move |c| chars.contains(c))(i)
}

pub fn char_checker<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
{
    input.split_at_position1_complete(
        |item| !(item.is_alphanum() || item == '_'),
        nom::error::ErrorKind::AlphaNumeric,
    )
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(char_checker, '\\', one_of("\"n\\"))(i)
}

fn parse_bool<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    println!("bool called : {}\n\n", i);
    let parse_true = value(true, tag("true"));
    let parse_false = value(false, tag("false"));

    alt((parse_true, parse_false)).parse(i)
}

fn parse_null<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    println!("null called : {}\n\n", input);
    value((), tag("None")).parse(input)
}

fn parse_string<'a, E: ParseError<&'a str> + ContextError<&'a str> + std::fmt::Debug>(
    input: &'a str,
) -> IResult<&'a str, &'a str, E> {
    context(
        "string",
        preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
    )(input)
}

fn parse_integer<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, isize, E> {
    let (number, data) = opt(char('-'))(input)?;
    digit1(number).and_then(|(rest, doq)| match (doq.parse::<isize>(), data.is_some()) {
        (Ok(x), _) => Ok((rest, x)),
        (Result::Err(_), true) => Err(nom::Err::Failure(E::from_error_kind(
            input,
            nom::error::ErrorKind::Fail,
        ))),
        (Result::Err(_), false) => Err(nom::Err::Error(E::from_error_kind(
            input,
            nom::error::ErrorKind::Fail,
        ))),
    })
}

fn parse_float<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, f64, E> {
    println!("double called : {}\n\n", input);
    double(input)
}

fn parse_array<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, Vec<DataModel<'a>>, E> {
    println!("array called : {}\n\n", input);
    context(
        "array",
        preceded(
            char('['),
            cut(terminated(
                separated_list0(preceded(spacer, char(',')), data_model),
                preceded(spacer, char(']')),
            )),
        ),
    )
    .parse(input)
}

fn parse_array_tuple<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, Vec<DataModel<'a>>, E> {
    println!("array tuple called : {}\n\n", input);
    context(
        "tuple",
        preceded(
            char('('),
            cut(terminated(
                separated_list0(preceded(spacer, char(',')), data_model),
                preceded(spacer, char(')')),
            )),
        ),
    )
    .parse(input)
}

fn parse_key_value_hash<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    i: &'a str,
) -> IResult<&'a str, (&'a str, DataModel<'a>), E> {
    separated_pair(
        preceded(spacer, parse_string),
        cut(preceded(spacer, char(':'))),
        preceded(spacer, data_model),
    )
    .parse(i)
}

fn parse_key_value_struct<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    i: &'a str,
) -> IResult<&'a str, (&'a str, DataModel<'a>), E> {
    println!("kv struct hash: {}", i);
    separated_pair(
        preceded(spacer, parse_str),
        cut(preceded(spacer, char(':'))),
        preceded(spacer, data_model),
    )
    .parse(i)
}

fn parse_hash<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, HashMap<&'a str, DataModel<'a>>, E> {
    println!("hashmap called : {}\n\n", input);
    context(
        "map",
        preceded(
            char('{'),
            cut(terminated(
                map(
                    separated_list0(preceded(spacer, char(',')), parse_key_value_hash),
                    |tuple_vec| tuple_vec.into_iter().collect(),
                ),
                preceded(spacer, char('}')),
            )),
        ),
    )(input)
}

fn parse_hash_unticked<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, HashMap<&'a str, DataModel<'a>>, E> {
    println!("unticked hash: {}", input);
    context(
        "struct map",
        preceded(
            spacer,
            preceded(
                char('{'),
                cut(terminated(
                    map(
                        separated_list0(preceded(spacer, char(',')), parse_key_value_struct),
                        |tuple_vec| tuple_vec.into_iter().collect(),
                    ),
                    preceded(spacer, char('}')),
                )),
            ),
        ),
    )(input)
}

fn parse_struct<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, HashMap<&'a str, DataModel<'a>>, E> {
    println!("struct called : {}\n\n", input);
    let value = context(
        "struct",
        separated_pair(parse_str, spacer, parse_hash_unticked),
    )(input);
    println!("post parse struct: {:#?}", value);
    let value = value?;

    Ok((value.0, value.1 .1))
}

fn parse_tuple_var<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    input: &'a str,
) -> IResult<&'a str, DataModel<'a>, E> {
    context(
        "option",
        preceded(
            preceded(parse_str, char('(')),
            cut(terminated(data_model, char(')'))),
        ),
    )(input)
}

pub fn char_checker_wc<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
    <&'a str as nom::InputTakeAtPosition>::Item: nom::AsChar,
{
    input.split_at_position1_complete(
        |item| item == ',' || item == '}',
        nom::error::ErrorKind::AlphaNumeric,
    )
}

fn parse_wildcard<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(char_checker_wc, '\\', one_of("\"n\\"))(i)
}

fn data_model<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    i: &'a str,
) -> IResult<&'a str, DataModel<'a>, E> {
    // println!("data model: {}\n\n", i);
    preceded(
        spacer,
        alt((
            map(parse_null, |_| DataModel::Null),
            map(parse_bool, DataModel::Boolean),
            map(parse_float, DataModel::Float),
            map(string::parse_string, DataModel::String),
            map(parse_array_tuple, DataModel::Vec),
            map(parse_array, DataModel::Vec),
            map(parse_hash, DataModel::Map),
            map(parse_tuple_var, |x| x),
            map(parse_struct, DataModel::Map),
            map(parse_wildcard, |x| DataModel::String(x.to_string())),
        )),
    )
    .parse(i)
}

fn root<
    'a,
    E: ParseError<&'a str>
        + ContextError<&'a str>
        + FromExternalError<&'a str, std::num::ParseIntError>
        + std::fmt::Debug,
>(
    i: &'a str,
) -> IResult<&'a str, DataModel<'a>, E> {
    delimited(spacer, data_model, opt(spacer)).parse(i)
}

#[cfg(test)]
mod tests {

    use nom::error::ErrorKind;

    use crate::*;

    #[derive(Debug)]
    struct Everything {
        integer: i32,
        uint: u32,
        float: f64,
        string: String,
        vector_int: Vec<i32>,
        vector_str: Vec<String>,
        hashmap: HashMap<String, i32>,
        nested: Bob,
        custom_hidden: Hidden,
        enumer1: Boat,
        enumer2: Boat,
        enumer3: Option<Boat>,
        tutu: (i32, f64),
        nothing: Option<()>,
        boolean: bool,
    }

    #[derive(Debug)]
    enum Boat {
        JustOne(i32),
        AnCouple((i32, String)),
        JustStruct { names: Vec<String>, age: i32 },
    }

    #[derive(Debug)]
    struct Bob {
        inner_int: f32,
        inner_string: String,
    }

    struct Hidden;

    impl std::fmt::Debug for Hidden {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("*** hidden ***")
        }
    }

    fn generate_data() -> Everything {
        Everything {
            uint: 321,
            integer: -123,
            float: 123.456,
            string: "Bob said, \"Hello!\"".to_owned(),
            vector_int: vec![12, 45, 56, -1, -3],
            vector_str: vec![
                "Alice".to_string(),
                "Venus".to_string(),
                "Karter".to_string(),
            ],
            hashmap: [
                ("Draco".to_string(), 123),
                ("Harry".to_string(), -123),
                ("Ron".to_string(), 0),
            ]
            .into_iter()
            .collect(),
            nested: Bob {
                inner_int: -50.0,
                inner_string: "Sharel".to_string(),
            },
            custom_hidden: Hidden,
            enumer1: Boat::JustOne(1024),
            enumer2: Boat::AnCouple((512, "Freak".to_string())),
            enumer3: Some(Boat::JustStruct {
                names: vec!["Tricky".to_string(), "Hacky".to_string()],
                age: -256,
            }),
            tutu: (12, -12.5),
            nothing: None,
            boolean: false,
        }
    }

    #[test]
    #[ignore]
    fn test_generate() {
        panic!("{:?}", generate_data())
    }

    #[test]
    #[ignore]
    fn debug_test() {
        let data = "!13113431";
        let (x, y) = parse_integer::<()>(data).unwrap();
        panic!("{:#?}", (x, y))
    }

    #[test]
    fn test_null() {
        let data = "null";
        let value = parse_null::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, (), "residue {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_null() {
        let data = "123";
        let value = parse_null::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, (), "residue {}", value.0)
    }

    #[test]
    fn test_boolean() {
        let data = "true";
        let value = parse_bool::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, true, "residue: {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_bool() {
        let data = "123";
        let _value = parse_bool::<(&str, ErrorKind)>(data).unwrap();
    }

    #[test]
    fn test_string() {
        let data = r#""true""#;
        let value = string::parse_string::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, "true", "residue: {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_string() {
        let data = "true";
        let _value = string::parse_string::<(&str, ErrorKind)>(data).unwrap();
    }

    #[test]
    fn test_float() {
        let data = r#"123.35"#;
        let value = double::<_, (&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, 123.35, "residue: {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_float() {
        let data = r#""213""#;
        let _value = double::<_, (&str, ErrorKind)>(data).unwrap();
    }

    #[test]
    fn test_integer() {
        let data = r#"123"#;
        let value = parse_integer::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, 123, "residue: {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_integer() {
        let data = r#""213""#;
        let _value = parse_integer::<(&str, ErrorKind)>(data).unwrap();
    }

    #[test]
    fn test_array() {
        let data = "[ \"12\", 2.3]";
        eprintln!("{}", data);
        let value = parse_array::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            vec![DataModel::String("12".to_string()), DataModel::Float(2.3)],
            "residue: {}",
            value.0
        )
    }

    #[test]
    #[should_panic]
    fn test_not_array() {
        let data = "[ \"12\"; 23]";
        let value = parse_array::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            vec![DataModel::String("12".to_string()), DataModel::Float(23.0)],
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_array_tuple() {
        let data = "(\"12\",23)";
        let value = parse_array_tuple::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            vec![DataModel::String("12".to_string()), DataModel::Float(23.0)],
            "residue: {}",
            value.0
        )
    }

    #[test]
    #[should_panic]
    fn test_not_array_tuple() {
        let data = "( \"12\"; 23)";
        let value = parse_array_tuple::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            vec![DataModel::String("12".to_string()), DataModel::Float(23.0)],
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_hash() {
        let data = r#"{ inner: "data", outer: 123 }"#;
        let value = parse_hash::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            [
                ("inner", DataModel::String("data".to_string())),
                ("outer", DataModel::Float(123.0))
            ]
            .into_iter()
            .collect(),
            "residue: {}",
            value.0
        )
    }

    #[test]
    #[should_panic]
    fn test_not_hash() {
        let data = r#"{ inner: "data", outer: 123, value: {} }"#;
        let value = parse_hash::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            [
                ("inner", DataModel::String("data".to_string())),
                ("outer", DataModel::Float(123.0))
            ]
            .into_iter()
            .collect(),
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_struct() {
        let data = r#"Yager { inner: "data", outer: 123 }"#;
        let value = parse_struct::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            [
                ("inner", DataModel::String("data".to_string())),
                ("outer", DataModel::Float(123.0))
            ]
            .into_iter()
            .collect(),
            "residue: {}",
            value.0
        )
    }

    #[test]
    #[should_panic]
    fn test_not_struct() {
        let data = r#"Insider( inner: "data", outer: 123, value: {} )"#;
        let value = parse_struct::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            [
                ("inner", DataModel::String("data".to_string())),
                ("outer", DataModel::Float(123.0))
            ]
            .into_iter()
            .collect(),
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_array_tuple_var() {
        let data = "Data((\"12\",23))";
        let value = parse_tuple_var::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            DataModel::Vec(vec![
                DataModel::String("12".to_string()),
                DataModel::Float(23.0)
            ]),
            "residue: {}",
            value.0
        )
    }

    #[test]
    #[should_panic]
    fn test_not_array_tuple_var() {
        let data = "Data( \"12\", 23)";
        let value = parse_tuple_var::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(
            value.1,
            DataModel::Vec(vec![
                DataModel::String("12".to_string()),
                DataModel::Float(23.0)
            ]),
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_bob() {
        let bob = Bob {
            inner_int: 123.0,
            inner_string: "data".to_string(),
        };

        let val = format!("{:?}", bob);
        assert_eq!(
            serde_json::to_string(&root::<(&str, ErrorKind)>(&val).unwrap().1).unwrap(),
            "{\"inner_int\":123.0,\"inner_string\":\"data\"}",
        );
    }

    #[test]
    fn test_try_all() {
        let data = generate_data();
        let data = format!("{:?}", data);
        eprintln!("{:#?}", data);
        let data_model = root::<(&str, ErrorKind)>(&data).unwrap().1;
        eprintln!("{:#?}", data_model);
        panic!()
    }

    #[derive(Debug)]
    struct A {
        data: String,
        value: Ba,
    }
    #[derive(Debug)]
    struct Ba {
        item: i32,
    }

    #[test]
    fn test_xyz() {
        let data = A {
            data: "123".to_string(),
            value: Ba { item: 123 },
        };
        let data = format!("{:?}", data);
        let data_model = root::<(&str, ErrorKind)>(&data).unwrap().1;
        eprintln!("{:#?}", data_model);
        panic!()
    }
}
