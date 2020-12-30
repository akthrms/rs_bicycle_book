use toy_vec::ToyVec;

fn main() {
    let mut v = ToyVec::new();
    v.push("Java Finch".to_string());
    v.push("Budgeringar".to_string());

    let mut iter = v.iter();

    assert_eq!(iter.next(), Some(&"Java Finch".to_string()));
    v.push("Canary".to_string());
}
