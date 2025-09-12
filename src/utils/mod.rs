use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};

#[derive(Debug)]
pub struct NiceError {
    message: String,
}

impl NiceError {
    pub fn new(message: String) -> NiceError {
        let error = NiceError { message };
        error
    }

    pub fn show(self) -> NiceError {
        eprintln!("{}", self.message);
        self
    }
}

pub fn read_file(filename: &String) -> Result<String, NiceError> {
    let mut file = match OpenOptions::new().read(true).open(filename) {
        Ok(file) => file,
        Err(error) => {
            return Err(NiceError::new(format!("Error opening file: {:?}", error)));
        }
    };

    let mut contents = String::new();

    let _ = match file.read_to_string(&mut contents) {
        Ok(_) => Ok(()),
        Err(_) => Err(NiceError::new(format!(
            "Error opening file: {:?}",
            filename
        ))),
    };

    Ok(contents)
}

pub fn read_lines(filename: &String) -> Result<Vec<String>, NiceError> {
    let contents = read_file(&filename)?;
    let lines: Vec<String> = contents.lines().map(String::from).collect();
    Ok(lines)
}

pub fn write_lines_to_file(filename: &str, lines: Vec<String>) -> io::Result<()> {
    let mut file = File::create(filename)?;

    for line in lines {
        writeln!(file, "{}", line)?;
    }

    Ok(())
}

pub fn pascal(s: Vec<String>) -> String {
    s.into_iter()
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => {
                    first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase()
                }
                None => String::new(),
            }
        })
        .collect::<String>()
}

pub fn snake(s: Vec<String>) -> String {
    s.into_iter()
        .map(|word| word.to_lowercase())
        .collect::<Vec<_>>()
        .join("_")
}
