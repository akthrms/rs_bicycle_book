use regex::Regex;
use std::collections::HashMap;
use std::io::BufRead;

pub fn count(input: impl BufRead, option: CountOption) -> HashMap<String, usize> {
    let regex = Regex::new(r"\w+").unwrap();
    let mut counts = HashMap::new();

    for line in input.lines() {
        let line = line.unwrap();

        match option {
            CountOption::Char => {
                for c in line.chars() {
                    *counts.entry(c.to_string()).or_insert(0) += 1;
                }
            }
            CountOption::Word => {
                for m in regex.find_iter(&line) {
                    let word = m.as_str().to_string();
                    *counts.entry(word).or_insert(0) += 1;
                }
            }
            CountOption::Line => {
                *counts.entry(line.to_string()).or_insert(0) += 1;
            }
        }
    }

    counts
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CountOption {
    Char,
    Word,
    Line,
}

impl Default for CountOption {
    fn default() -> Self {
        CountOption::Word
    }
}
