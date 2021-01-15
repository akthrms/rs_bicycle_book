use std::env;
use std::fs::File;
use std::io::BufReader;

use wordcount::count;

fn main() {
    let filename = env::args().nth(1).expect("1 argument FILENAME required");
    let file = File::open(filename).unwrap();
    let reader = BufReader::new(&file);

    let counts = count(reader, Default::default());
    println!("{:?}", counts);
}
