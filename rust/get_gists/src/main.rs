use serde_json as json;

type StdResult<T> = Result<T, Box<dyn std::error::Error>>;

fn main() -> StdResult<()> {
    //
    let gists: json::Value = ureq::get("https://api.github.com/users/ivan2029/gists")
        .set("Accept", "application/vnd.github.v3+json")
        .call()?
        .into_json()?;

    //
    for gist in gists.as_array().unwrap() {        
        let files = gist.get("files")
            .unwrap()
            .as_object()
            .unwrap();

        for (key, val) in files {
            println!("{}", key);

            let raw_url = val.get("raw_url")
                .unwrap()
                .as_str()
                .unwrap();

            let contents: String = ureq::get(raw_url)
                .call()?
                .into_string()?;

            {
                use std::io::Write;
                
                let file = std::fs::File::create(format!("gists/{}", key))?;
                let mut writer = std::io::BufWriter::new(file);
                writer.write(contents.as_bytes())?;
            }  
        }
    }


    //
    Ok(())
}

