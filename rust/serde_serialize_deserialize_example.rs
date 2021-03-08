use anyhow::Result;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_yaml as yaml;

fn main() -> Result<()> {
    //
    let command = Command {
        id: 123,
        method: Method::Read {
            path: "hello/path".into(),
        },
    };

    let serialized = yaml::to_string(&command)?;

    println!("{}", serialized);

    execute_command(&command);

    //
    let command = Command {
        id: 123,
        method: Method::Write {
            path: "hello/path".into(),
            data: "abcdefgh...".into(),
        },
    };

    let serialized = yaml::to_string(&command)?;

    println!("{}", serialized);

    execute_command(&command);

    //
    let serialized_command = r#"
---
id: 123
method: write
args:
  path: hello/path
  data: YWJjZGVmZ2guLi4=  # base64_encode("abcdefgh...")
"#;

    let deserialized_command: Command = yaml::from_str(&serialized_command)?;

    println!("{:#?}", deserialized_command);

    if let Method::Write { ref data, .. } = &deserialized_command.method {
        let data = std::str::from_utf8(data);
        println!("{:?}", data);
    }

    //
    Ok(())
}

//
//
//
#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "lowercase", tag = "method", content = "args")]
enum Method {
    Read {
        path: String,
    },
    Write {
        path: String,
        #[serde(
            serialize_with = "serialize_bytes_as_base64",
            deserialize_with = "deserialize_base64_to_bytes"
        )]
        data: Vec<u8>,
    },
}

#[derive(Serialize, Deserialize, Debug)]
struct Command {
    id: u32,
    #[serde(flatten)]
    method: Method,
}

fn serialize_bytes_as_base64<S>(value: &[u8], serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    use base64::encode;

    let encoded = encode(value);

    serializer.serialize_str(&encoded)
}

fn deserialize_base64_to_bytes<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
where
    D: Deserializer<'de>,
{
    use serde::de::Error;
    String::deserialize(deserializer)
        .and_then(|string| base64::decode(&string).map_err(|err| Error::custom(err.to_string())))
}

//
//
//
fn execute_command(command: &Command) {
    match command.method {
        Method::Read { ref path } => execute_read(path),
        Method::Write { ref path, ref data } => execute_write(path, data),
    }
}

fn execute_read(path: &str) {
    println!("reading {}", path);
}

fn execute_write(path: &str, data: &[u8]) {
    println!("writing {} bytes to {}", data.len(), path);
}
