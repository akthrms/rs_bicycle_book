use toy_vec::ToyVec;

fn main() {
    let e: Option<&String>;
    {
        let mut v = ToyVec::new();
        v.push("Java Finch".to_string());
        v.push("Budgeringar".to_string());
        let e = v.get(1);
    }
    assert_eq!(e, Some(&"Budgeringar".to_string()));
}
