use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn load_program(filename: &str) -> Vec<i32> {
    let file = File::open(filename).expect("File not found.");
    let mut reader = BufReader::new(file);

    let mut prog: String = String::new();
    reader
        .read_line(&mut prog)
        .expect("Error reading line from file.");

    return prog.trim().split(",").map(|x| x.parse::<i32>().expect("Oh no.")).collect();
}

fn run_program(program: &mut Vec<i32>, noun: i32, verb: i32) -> i32 {
    program[1] = noun;
    program[2] = verb;

    let mut pos: usize = 0;
    loop {
        let opcode: i32 = program[pos];
        if opcode == 99 {
            return program[0];
        }

        if let [arg1, arg2, dest] = program[pos+1..pos+4] {
            if opcode == 1 {
                program[dest as usize] = program[arg1 as usize] + program[arg2 as usize];
            } else if opcode == 2 {
                program[dest as usize] = program[arg1 as usize] * program[arg2 as usize];
            } else {
                panic!("Unrecognised opcode {opcode}.");
            }
        } else {
            panic!("Could not parse args.")
        }
        pos += 4;
    }

}

fn main() {
    let filename: &str = "../input.txt";
    let mut program = load_program(filename);
    let result: i32 = run_program(&mut program, 12, 2);
    println!("Part 1: {result}");
}
