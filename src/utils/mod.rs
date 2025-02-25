use std::fs::OpenOptions;
use std::io::Read;

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
