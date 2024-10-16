use itertools::Itertools;
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
}

impl IntCode {
    fn new(filename: &str) -> IntCode {
        let mut intcode = IntCode {
            program: Vec::<String>::new(),
            op_arg_nums: HashMap::<char, usize>::new(),
            op_has_dest: HashMap::<char, bool>::new(),
            _position: 0,
        };

        intcode.op_arg_nums.insert('1', 2);
        intcode.op_arg_nums.insert('2', 2);
        intcode.op_arg_nums.insert('3', 0);
        intcode.op_arg_nums.insert('4', 1);
        intcode.op_arg_nums.insert('5', 2);
        intcode.op_arg_nums.insert('6', 2);
        intcode.op_arg_nums.insert('7', 2);
        intcode.op_arg_nums.insert('8', 2);

        intcode.op_has_dest.insert('1', true);
        intcode.op_has_dest.insert('2', true);
        intcode.op_has_dest.insert('3', true);
        intcode.op_has_dest.insert('4', false);
        intcode.op_has_dest.insert('5', false);
        intcode.op_has_dest.insert('6', false);
        intcode.op_has_dest.insert('7', true);
        intcode.op_has_dest.insert('8', true);

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

    fn print_program(&self) {
        for el in &self.program {
            print!("{}, ", el);
        }
        print!("\n");
    }

    fn run(
        &mut self,
        input_buffer: &mut VecDeque<i32>,
        output_buffer: &mut VecDeque<i32>,
    ) -> Option<i32> {
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

            let increment =
                self.process_operation(opcode, param_modes, input_buffer, output_buffer);

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
        input_buffer: &mut VecDeque<i32>,
        output_buffer: &mut VecDeque<i32>,
    ) -> Option<usize> {
        let num_args = self.op_arg_nums[opcode];
        let mut increment = num_args + 1;

        if param_modes.len() != num_args {
            for _ in 0..(num_args - param_modes.len()) {
                param_modes.push(&'0');
            }
        }
        if self.op_has_dest[opcode] {
            param_modes.push(&'1');
            increment += 1;
        }

        let mut args = Vec::<i32>::new();
        for (i, c) in param_modes.iter().enumerate() {
            let v = self.program[self._position + i + 1]
                .parse::<i32>()
                .expect("Error parsing string to int.");
            if **c == '0' {
                args.push(
                    self.program[v as usize]
                        .parse::<i32>()
                        .expect("Error parsing int"),
                );
            } else {
                args.push(v);
            }
        }

        if *opcode == '1' {
            self.program[args[2] as usize] = (args[0] + args[1]).to_string();
        } else if *opcode == '2' {
            self.program[args[2] as usize] = (args[0] * args[1]).to_string();
        } else if *opcode == '3' {
            if input_buffer.is_empty() {
                return None;
            }
            let input = input_buffer.pop_front().expect("No inputs to read.");
            self.program[args[0] as usize] = input.to_string();
        } else if *opcode == '4' {
            output_buffer.push_back(args[0]);
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
        } else {
            panic!("What is this opcode? {}", opcode);
        }

        return Some(increment);
    }
}

fn run_permutation(filename: &str, phases: Vec<i32>) -> Result<i32, &str> {
    const N: usize = 5;

    let mut intcodes: Vec<IntCode> = vec![IntCode::new(filename); N];
    let mut buffers: Vec<VecDeque<i32>> = vec![VecDeque::<i32>::new(); N];

    if phases.len() != N {
        return Err("Wrong number of phase values.");
    }

    for (v, phase) in &mut buffers.iter_mut().zip(phases) {
        v.push_back(phase);
    }
    buffers[0].push_back(0);

    let mut signal: Option<i32>;
    let mut intcode: &mut IntCode;
    for i in (0..N).cycle() {
        intcode = &mut intcodes[i];

        let input_buffer: &mut VecDeque<i32>;
        let output_buffer: &mut VecDeque<i32>;
        if i < 4 {
            let (input_buffers, output_buffers) = buffers.split_at_mut(i + 1);
            input_buffer = &mut input_buffers[input_buffers.len() - 1];
            output_buffer = &mut output_buffers[0];
        } else {
            let (output_buffers, input_buffers) = buffers.split_at_mut(i);
            input_buffer = &mut input_buffers[0];
            output_buffer = &mut output_buffers[0];
        }
        signal = intcode.run(input_buffer, output_buffer);

        if signal.is_some() && i == N - 1 {
            return signal.ok_or("No value in signal.");
        }
    }
    return Err("We shouldn't get here...");
}

fn run_sequence(filename: &str, phases: Vec<i32>) -> Result<i32, &str> {
    const N: usize = 5;

    let mut intcodes: Vec<IntCode> = vec![IntCode::new(filename); N];
    let mut buffers: Vec<VecDeque<i32>> = vec![VecDeque::<i32>::new(); N + 1];

    if phases.len() != N {
        return Err("Wrong number of phase values.");
    }

    for (v, phase) in &mut buffers.iter_mut().zip(phases) {
        v.push_back(phase);
    }
    buffers[0].push_back(0);

    let mut signal: Option<i32>;
    let mut intcode: &mut IntCode;
    for i in 0..N {
        intcode = &mut intcodes[i];

        let input_buffer: &mut VecDeque<i32>;
        let output_buffer: &mut VecDeque<i32>;

        let (input_buffers, output_buffers) = buffers.split_at_mut(i + 1);
        input_buffer = &mut input_buffers[input_buffers.len() - 1];
        output_buffer = &mut output_buffers[0];

        signal = intcode.run(input_buffer, output_buffer);

        if signal.is_some() && i == N - 1 {
            return signal.ok_or("No value in signal.");
        }
    }
    return Err("We shouldn't get here...");
}

fn main() {
    let filename: &str = "../input.txt";

    let signal_p1: i32 = (0..5)
        .permutations(5)
        .map(|phases| run_sequence(filename, phases).unwrap())
        .collect::<Vec<i32>>()
        .into_iter()
        .max()
        .unwrap();
    println!("Part I:  {:?}", &signal_p1);

    let signal_p2: i32 = (5..10)
        .permutations(5)
        .map(|phases| run_permutation(filename, phases).unwrap())
        .collect::<Vec<i32>>()
        .into_iter()
        .max()
        .unwrap();
    println!("Part II: {:?}", &signal_p2);
}
