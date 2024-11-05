use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Clone)]
struct IntCode {
    program: Vec<String>,
    op_arg_nums: HashMap<char, usize>,
    op_has_dest: HashMap<char, bool>,
    _position: usize,
    _relative_base: i64,
}

impl IntCode {
    fn new(filename: &str) -> IntCode {
        let mut intcode = IntCode {
            program: Vec::<String>::new(),
            op_arg_nums: HashMap::<char, usize>::new(),
            op_has_dest: HashMap::<char, bool>::new(),
            _position: 0,
            _relative_base: 0,
        };

        intcode.op_arg_nums.insert('1', 2);
        intcode.op_arg_nums.insert('2', 2);
        intcode.op_arg_nums.insert('3', 0);
        intcode.op_arg_nums.insert('4', 1);
        intcode.op_arg_nums.insert('5', 2);
        intcode.op_arg_nums.insert('6', 2);
        intcode.op_arg_nums.insert('7', 2);
        intcode.op_arg_nums.insert('8', 2);
        intcode.op_arg_nums.insert('9', 1);

        intcode.op_has_dest.insert('1', true);
        intcode.op_has_dest.insert('2', true);
        intcode.op_has_dest.insert('3', true);
        intcode.op_has_dest.insert('4', false);
        intcode.op_has_dest.insert('5', false);
        intcode.op_has_dest.insert('6', false);
        intcode.op_has_dest.insert('7', true);
        intcode.op_has_dest.insert('8', true);
        intcode.op_has_dest.insert('9', false);

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
        self.program.append(&mut vec![String::from("0"); 10000]);
    }

    fn run(
        &mut self,
        input_buffer: &mut VecDeque<i64>,
        output_buffer: &mut VecDeque<i64>,
    ) -> Option<i64> {
        loop {
            let instruction: &Vec<char> = &self.program[self._position].chars().collect();
            let opcode: &char;
            let param_modes: Vec<&char>;

            if instruction.len() > 1 {
                let opcode_set: &[char] = &instruction[&instruction.len() - 2..];
                if opcode_set == ['9', '9'] {
                    return Some(output_buffer[output_buffer.len() - 1]);
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

            let increment = self.process_operation(opcode, param_modes, input_buffer, output_buffer);

            if increment.is_some() {
                self._position += increment.unwrap();
            } else {
                return None;
            }
        }
    }

    fn process_operation(
        &mut self,
        opcode: &char,
        mut param_modes: Vec<&char>,
        input_buffer: &mut VecDeque<i64>,
        output_buffer: &mut VecDeque<i64>,
    ) -> Option<usize> {
        let num_args = self.op_arg_nums[opcode];
        let mut increment = num_args + 1;

        if param_modes.len() < num_args {
            param_modes.append(&mut vec![&'0'; num_args - param_modes.len()]);
        }

        let mut args = Vec::<i64>::new();
        for (i, c) in param_modes[0..num_args].iter().enumerate() {
            let v = self.program[self._position + i + 1]
                .parse::<i64>()
                .expect("Error parsing string to int.");

            args.push(
                match **c {
                    '0' => self.program[v as usize].parse::<i64>().expect("Error parsing int"),
                    '1' => v,
                    '2' => self.program[(self._relative_base + v) as usize].parse::<i64>().expect("Error parsing int"),
                    _ => panic!("Unrecognised param mode {}", c),
                }
            );
        }

        if self.op_has_dest[opcode] {
            let mut dest = self.program[self._position + num_args + 1]
                .parse::<i64>()
                .expect("Could not parse");

            if param_modes.len() > num_args && param_modes[num_args] == &'2' {
                dest += self._relative_base;
            }
            args.push(dest);
            increment += 1;
        }

        if *opcode == '1' {
            self.program[args[2] as usize] = (args[0] + args[1]).to_string();
        } else if *opcode == '2' {
            self.program[args[2] as usize] = (args[0] * args[1]).to_string();
        } else if *opcode == '3' {
            if input_buffer.is_empty() {
                println!("Empty Buffer");
                return None;
            }
            let input = input_buffer.pop_front().expect("No inputs to read.");
            self.program[args[0] as usize] = input.to_string();
        } else if *opcode == '4' {
            output_buffer.push_back(args[0]);
            println!("{}", args[0]);
        } else if *opcode == '5' || *opcode == '6' {
            if (*opcode == '5' && args[0] != 0) || (*opcode == '6' && args[0] == 0) {
                self._position = usize::try_from(args[1]).expect("Error converting to usize.");
                increment = 0;
            }
        } else if *opcode == '7' {
            self.program[args[2] as usize] = if args[0] < args[1] {
                String::from('1')
            } else {
                String::from('0')
            };
        } else if *opcode == '8' {
            self.program[args[2] as usize] = if args[0] == args[1] {
                String::from('1')
            } else {
                String::from('0')
            };
        } else if *opcode == '9' {
            self._relative_base += args[0];
        } else {
            panic!("What is this opcode? {}", opcode);
        }

        return Some(increment);
    }
}

fn main() {
    let filename: &str = "../input.txt";
    let mut intcode = IntCode::new(filename);

    let mut input = VecDeque::<i64>::new();
    input.push_back(1);
    let mut output = VecDeque::<i64>::new();
    let output = intcode.run(&mut input, &mut output);
    println!("Part 1: {}", output.expect("There should be a value"));
    
    let mut intcode = IntCode::new(filename);
    let mut input = VecDeque::<i64>::new();
    input.push_back(2);
    let mut output = VecDeque::<i64>::new();
    let output = intcode.run(&mut input, &mut output);
    println!("Part 2: {}", output.expect("There should be a value"));
}
