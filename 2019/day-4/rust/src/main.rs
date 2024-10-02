use std::fs::File;
use std::io::BufReader;
use std::io::Read;

fn load_data(filename: &str) -> [u32; 2] {
    let file: File = File::open(filename).expect("File not found.");
    let mut reader = BufReader::new(file);

    let mut lines: String = String::new();
    let _ = reader.read_to_string(&mut lines);
    let range: Vec<u32> = lines
        .split("-")
        .map(|n: &str| n.trim().parse().expect("Could not parse integer."))
        .collect();
    return [range[0], range[1]];
}

fn check_password(password: u32) -> bool {
    let digits: Vec<i64> = password
        .to_string()
        .chars()
        .map(|c: char| i64::from(c.to_digit(10).expect("digit parse error")))
        .collect();

    let diff: Vec<i64> = digits
        .iter()
        .zip(digits[1..].iter())
        .map(|p: (&i64, &i64)| p.1 - p.0)
        .collect();

    if diff.iter().any(|&d| d < 0) || !diff.iter().any(|&d| d == 0) {
        return false;
    }

    return true;
}

fn main() {
    let filename = "../input.txt";
    let range: [u32; 2] = load_data(filename);
    let num_valid: usize = (range[0]..range[1] + 1)
        .filter(|password| check_password(*password))
        .count();
    println!("Part 1: {num_valid}");
}
