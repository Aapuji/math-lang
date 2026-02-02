/// Holds configuration data for the compiler
#[derive(Debug)]
pub struct Config {
    file: String,
}

impl Config {
    pub fn new(path: String) -> Self {
        Self {
            file: path
        }
    }

    pub fn file(&self) -> &str {
        &self.file
    }
}
