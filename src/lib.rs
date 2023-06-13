use std::collections::HashMap;

use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while},
    character::complete::{alphanumeric1, char, digit1, one_of},
    combinator::{cut, map, map_res, opt, recognize, value},
    error::{context, ContextError, ParseError},
    multi::separated_list0,
    number::complete::{double, float},
    sequence::{delimited, preceded, separated_pair, terminated},
    Err, IResult, Parser,
};

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
#[serde(untagged)]
enum DataModel<'a> {
    Null,                                 // ✅
    Boolean(bool),                        // ✅
    Float(f64),                           // ✅
    String(&'a str),                      // ✅
    Map(HashMap<&'a str, DataModel<'a>>), //
    Vec(Vec<DataModel<'a>>),              //
}

fn spacer<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";

    take_while(move |c| chars.contains(c))(i)
}

fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn parse_bool<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, bool, E> {
    let parse_true = value(true, tag("true"));
    let parse_false = value(false, tag("false"));

    alt((parse_true, parse_false)).parse(i)
}

fn parse_null<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    value((), tag("null")).parse(input)
}

fn parse_string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
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

fn parse_array<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<DataModel<'a>>, E> {
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

fn parse_array_tuple<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, Vec<DataModel<'a>>, E> {
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

fn parse_key_value<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, (&'a str, DataModel<'a>), E> {
    separated_pair(
        preceded(spacer, parse_str),
        cut(preceded(spacer, char(':'))),
        data_model,
    )
    .parse(i)
}

fn parse_hash<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, HashMap<&'a str, DataModel<'a>>, E> {
    context(
        "map",
        preceded(
            char('{'),
            cut(terminated(
                map(
                    separated_list0(preceded(spacer, char(',')), parse_key_value),
                    |tuple_vec| tuple_vec.into_iter().collect(),
                ),
                preceded(spacer, char('}')),
            )),
        ),
    )(input)
}

fn parse_struct<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    input: &'a str,
) -> IResult<&'a str, HashMap<&'a str, DataModel<'a>>, E> {
    let value = context("struct", separated_pair(parse_str, spacer, parse_hash))(input)?;

    Ok(value.1)
}

fn parse_tuple_var<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
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

fn data_model<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, DataModel<'a>, E> {
    println!("{}", i);
    preceded(
        spacer,
        alt((
            map(parse_tuple_var, |x| x),
            map(parse_struct, DataModel::Map),
            map(parse_hash, DataModel::Map),
            map(parse_array, DataModel::Vec),
            map(parse_array_tuple, DataModel::Vec),
            map(parse_string, DataModel::String),
            map(double, DataModel::Float),
            map(parse_bool, DataModel::Boolean),
            map(parse_null, |_| DataModel::Null),
        )),
    )
    .parse(i)
}

fn root<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, DataModel<'a>, E> {
    delimited(spacer, data_model, opt(spacer)).parse(i)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use nom::error::ErrorKind;
    use serde::Deserialize;

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
        innerint: f32,
        innerstring: String,
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
                innerint: -50.0,
                innerstring: "Sharel".to_string(),
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
        let value = parse_string::<(&str, ErrorKind)>(data).unwrap();
        assert_eq!(value.1, "true", "residue: {}", value.0)
    }

    #[test]
    #[should_panic]
    fn test_not_string() {
        let data = "true";
        let _value = parse_string::<(&str, ErrorKind)>(data).unwrap();
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
            vec![DataModel::String("12"), DataModel::Float(2.3)],
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
            vec![DataModel::String("12"), DataModel::Float(23.0)],
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
            vec![DataModel::String("12"), DataModel::Float(23.0)],
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
            vec![DataModel::String("12"), DataModel::Float(23.0)],
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
                ("inner", DataModel::String("data")),
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
                ("inner", DataModel::String("data")),
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
                ("inner", DataModel::String("data")),
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
                ("inner", DataModel::String("data")),
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
            DataModel::Vec(vec![DataModel::String("12"), DataModel::Float(23.0)]),
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
            DataModel::Vec(vec![DataModel::String("12"), DataModel::Float(23.0)]),
            "residue: {}",
            value.0
        )
    }

    #[test]
    fn test_bob() {
        let bob = Bob {
            innerint: 123.0,
            innerstring: "data".to_string(),
        };

        let val = format!("{:?}", bob);
        assert_eq!(
            serde_json::to_string(&root::<(&str, ErrorKind)>(&val).unwrap().1).unwrap(),
            "{\"innerstring\":\"data\",\"innerint\":123.0}",
        );
    }
}
