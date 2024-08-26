use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

pub struct IntCode<'a> {
    program: Vec<String>,
    pub inputs: Vec<i32>,
    op_arg_nums: HashMap<&'a char, usize>,
    //pub outputs:&'a Vec<i32>,
    _position: usize,
}

impl IntCode<'_> {
    fn new(filename: &str, inputs: Vec<i32>) -> IntCode {
        let mut intcode = IntCode {
            program: Vec::<String>::new(),
            inputs: inputs,
            op_arg_nums: HashMap::<&char, usize>::new(),
            _position: 0,
        };

        intcode.op_arg_nums.insert(&'1', 3);
        intcode.op_arg_nums.insert(&'2', 3);
        intcode.op_arg_nums.insert(&'3', 1);
        intcode.op_arg_nums.insert(&'4', 1);
        intcode.op_arg_nums.insert(&'5', 2);
        intcode.op_arg_nums.insert(&'6', 2);
        intcode.op_arg_nums.insert(&'7', 3);
        intcode.op_arg_nums.insert(&'8', 3);

        intcode.load_program(filename);

        return intcode;
    }

    fn load_program(&mut self, filename: &str) {
        let file = File::open(filename).expect("File not found.");
        let mut reader = BufReader::new(file);

        let mut prog: String = String::new();
        reader
            .read_line(&mut prog)
            .expect("Error reading line from file.");

        self.program = prog.trim().split(",").map(|x| x.to_string()).collect();
    }

    pub fn print_program(&self) {
        for el in &self.program {
            print!("{}, ", el);
        }
        print!("\n");
    }

    pub fn run(&mut self) {
        loop {
            let instruction: &Vec<char> = &self.program[self._position].chars().collect();
            let opcode: &char;
            let param_modes: Vec<&char>;

            if instruction.len() > 1 {
                let opcode_set: &[char] = &instruction[&instruction.len() - 2..];
                if opcode_set == ['9', '9'] {
                    break;
                } else {
                    opcode = &opcode_set[opcode_set.len() - 1];
                }
            } else {
                opcode = &instruction[0];
            }

            if instruction.len() > 2 {
                param_modes = instruction[0..instruction.len() - 2].iter().rev().collect();
            } else {
                param_modes = Vec::<&char>::new();
            }

            let increment = self.process_operation(opcode, param_modes);

            self._position += increment;
        }
    }

    fn process_operation(&self, opcode: &char, mut param_modes: Vec<&char>) -> usize {
        let num_args = self.op_arg_nums[opcode];

        if param_modes.len() != num_args {
            for _ in 0..(num_args - param_modes.len()) {
                param_modes.push(&'0');
            }
        }

        let args = Vec::<i32>::new();
        for i in 1..num_args + 1 {
            args.push(self.program[self._position + 1].parse::<i32>().expect("Error parsing string to int."));
        }

        if param_modes[0] == &'1' {
            let val1: i32;
            let val2: i32;
            if param_modes[0] == &'0' {
                val1 = self.program[args[0]].parse::<i32>();
            }
            self.program[args[args.len()-1]] = val1 + val2;
        } else {
            arg1 =
        }
        arg1 = self.program[]

        return num_args + 1;
    }
}

fn main() {
    let filename: &str = "../test_input.txt";
    let mut intcode: IntCode = IntCode::new(filename, vec![0]);
    intcode.print_program();
    intcode.args.push()}
