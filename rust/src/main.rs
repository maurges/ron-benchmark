use ron::value::Value;

fn usage(execuable_name: std::string::String) {
    println!("Usage: {} FILENAME.ron\n\tParse RON file and measure time", execuable_name)
}

#[derive(Debug, serde::Deserialize, PartialEq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
enum MyEnumWithDashes {
    ThisIsMyUnitVariant,
    ThisIsMyTupleVariant(bool, i32),
}

#[derive(Debug, serde::Deserialize, PartialEq, serde::Serialize)]
#[serde(rename_all = "kebab-case")]
struct MyStructWithDashes {
    my_enum: MyEnumWithDashes,
    #[serde(rename = "2nd")]
    my_enum2: MyEnumWithDashes,
    will_be_renamed: u32,
}

fn roundtrip_ident_with_dash() {
    let value = MyStructWithDashes {
        my_enum: MyEnumWithDashes::ThisIsMyUnitVariant,
        my_enum2: MyEnumWithDashes::ThisIsMyTupleVariant(false, -3),
        will_be_renamed: 32,
    };

    let serial = ron::ser::to_string(&value).unwrap();

    println!("Serialized: {}", serial);

    let deserial = ron::de::from_str(&serial);

    assert_eq!(Ok(value), deserial);
}

fn main() {
    let mut args = std::env::args();
    let executable = args.next().unwrap();
    let name = match args.next() {
        None => return usage(executable),
        Some(arg) if arg == "--help" => return usage(executable),
        Some(arg) if arg == "test" => return roundtrip_ident_with_dash(),
        Some(name) => name,
    };
    let content = std::string::String::from_utf8(
        std::fs::read(&name).unwrap()
    ).unwrap();

    let start = std::time::Instant::now();

    let value = ron::from_str::<Value>(&content).unwrap();
    let depth = compute_depth(&value);

    let delta = start.elapsed();
    let microseconds = delta.as_micros();
    println!("{},{},{}", name, depth, microseconds)
}

fn compute_depth(v: &Value) -> i64 {
    fn go(v: &Value, r: i64) -> i64 {
        match v {
            Value::Bool(_) => r,
            Value::Char(_) => r,
            Value::Map(xs) => xs.values().map(|x| go(x, r+1)).max().unwrap_or(r),
            Value::Number(_) => r,
            Value::Option(Some(x)) => go(x, r),
            Value::Option(None) => r,
            Value::String(_) => r,
            Value::Seq(xs) => xs.iter().map(|x| go(x, r+1)).max().unwrap_or(r),
            Value::Unit => r,
        }
    }
    go(v, 1)
}
