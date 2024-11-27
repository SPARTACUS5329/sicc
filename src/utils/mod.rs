use std::fs::OpenOptions;
use std::io::{self, Read};

pub fn read_file(filename: &String) -> io::Result<String> {
    let mut file = match OpenOptions::new().read(true).open(filename) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Error opening file: {:?}", filename);
            return Err(error);
        }
    };

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

pub fn read_lines(filename: &String) -> io::Result<Vec<String>> {
    let contents = read_file(&filename)?;
    let lines: Vec<String> = contents.lines().map(String::from).collect();
    Ok(lines)
}
