use std::collections::HashMap;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use debug_parser::root;
use nom::error::ErrorKind;

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

fn bench_everything(c: &mut Criterion) {
    let data = generate_data();
    c.bench_function("bench_parser", |b| {
        b.iter(|| {
            let value = format!("{:?}", black_box(&data));
            let _ = black_box(root::<(&str, ErrorKind)>(&value)).unwrap().1;
        })
    });
}

fn bench_everything_dbg_pnt(c: &mut Criterion) {
    let data = generate_data();
    c.bench_function("bench_debug_print", |b| {
        b.iter(|| {
            let _ = format!("{:?}", black_box(&data));
        })
    });
}

fn bench_parse_and_serialize(c: &mut Criterion) {
    let data = generate_data();
    c.bench_function("bench_parse_and_serialize", |b| {
        b.iter(|| {
            let value = format!("{:?}", black_box(&data));
            let parsed = black_box(root::<(&str, ErrorKind)>(&value).unwrap().1);
        })
    });
}

criterion_group!(benches, bench_everything, bench_everything_dbg_pnt, bench_parse_and_serialize);
criterion_main!(benches);
