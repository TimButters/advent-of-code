//use factorial::Factorial;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd)]
struct Point {
    x: i64,
    y: i64,
}

struct Arm {
    pos: Point,
    coords: HashMap<char, Point>,
}

impl Arm {
    fn new(coord_map: HashMap<char, Point>) -> Arm {
        let start_pos = coord_map[&'A'].clone();
        return Arm {
            pos: start_pos,
            coords: coord_map,
        };
    }

    fn new_numpad() -> Arm {
        let coord_map: HashMap<char, Point> = HashMap::from([
            ('X', Point { x: 0, y: 3 }),
            ('0', Point { x: 1, y: 3 }),
            ('A', Point { x: 2, y: 3 }),
            ('1', Point { x: 0, y: 2 }),
            ('2', Point { x: 1, y: 2 }),
            ('3', Point { x: 2, y: 2 }),
            ('4', Point { x: 0, y: 1 }),
            ('5', Point { x: 1, y: 1 }),
            ('6', Point { x: 2, y: 1 }),
            ('7', Point { x: 0, y: 0 }),
            ('8', Point { x: 1, y: 0 }),
            ('9', Point { x: 2, y: 0 }),
        ]);
        return Arm::new(coord_map);
    }

    fn new_dirpad() -> Arm {
        let coord_map: HashMap<char, Point> = HashMap::from([
            ('X', Point { x: 0, y: 0 }),
            ('^', Point { x: 1, y: 0 }),
            ('A', Point { x: 2, y: 0 }),
            ('<', Point { x: 0, y: 1 }),
            ('v', Point { x: 1, y: 1 }),
            ('>', Point { x: 2, y: 1 }),
        ]);
        return Arm::new(coord_map);
    }

    fn move_arm(&mut self, target: char) {
        let destination: &Point = &self.coords[&target];
        //let x_diff: u64 = (self.pos.x - destination.x)
        //    .abs()
        //    .try_into()
        //    .expect("Could not convert to unsigned int.");
        //let y_diff: u64 = (self.pos.y - destination.y)
        //    .abs()
        //    .try_into()
        //    .expect("Could not convert to unsigned int.");

        self.pos = destination.clone();
    }
}

fn load_program(filename: &str) -> Vec<Vec<char>> {
    let file = File::open(filename).expect("File not found.");
    let reader = BufReader::new(file);

    let mut codes: Vec<Vec<char>> = vec![];
    for line in reader.lines() {
        let code: String = line.expect("Error reading line from file.");
        codes.push(code.trim().chars().collect());
    }

    return codes;
}

fn arm_moves_arm(arm: &mut Arm, target: &char) -> Vec<char> {
    let current_pos = arm.pos.clone();
    let target_coord = arm.coords[&target];
    let error_coord = arm.coords[&'X'];
    let x_change = target_coord.x - arm.pos.x;
    let y_change = target_coord.y - arm.pos.y;
    arm.move_arm(*target);

    let x_button: char;
    if x_change > 0 {
        x_button = '>';
    } else if x_change < 0 {
        x_button = '<';
    } else {
        x_button = 'X';
    }

    let y_button: char;
    if y_change > 0 {
        y_button = 'v';
    } else if y_change < 0 {
        y_button = '^';
    } else {
        y_button = 'X';
    }

    let button_1: char;
    let button_2: char;
    let change_1: i64;
    let change_2: i64;
    let inter_coord = Point {
        x: current_pos.x + x_change,
        y: current_pos.y,
    };
    if inter_coord == error_coord {
        button_1 = y_button;
        change_1 = y_change.abs();
        button_2 = x_button;
        change_2 = x_change.abs();
    } else {
        button_1 = x_button;
        change_1 = x_change.abs();
        button_2 = y_button;
        change_2 = y_change.abs();
    }

    let mut sequence: Vec<char> = vec![];
    for _ in 0..change_1 {
        sequence.push(button_1);
    }
    for _ in 0..change_2 {
        sequence.push(button_2);
    }
    sequence.push('A');
    return sequence;
}

fn process_movement(arm: &mut Arm, target: &Vec<char>) -> Vec<char> {
    let mut seqs: Vec<Vec<char>> = vec![];
    for c in target {
        let seq: Vec<char> = arm_moves_arm(arm, &c);
        seqs.push(seq);
    }
    let sequence = seqs.into_iter().flatten().collect::<Vec<char>>();
    let s: String = sequence.iter().collect();
    println!("{}: {s}", sequence.len());
    return sequence;
}

fn enter_code(target: &Vec<char>) -> usize {
    let mut numpad = Arm::new_numpad();
    let mut dirpad1 = Arm::new_dirpad();
    let mut dirpad2 = Arm::new_dirpad();

    let seq1: Vec<char> = process_movement(&mut numpad, &target);
    let seq2: Vec<char> = process_movement(&mut dirpad1, &seq1);
    let seq3: Vec<char> = process_movement(&mut dirpad2, &seq2);

    let numeric_part: usize = target
        .get(..target.len() - 1)
        .expect("Error with get method")
        .iter()
        .collect::<String>()
        .parse::<usize>()
        .expect("Could not parse to int");
    println!("{} {}", seq3.len(), numeric_part);
    return seq3.len() * numeric_part;
}

fn main() {
    let targets: Vec<Vec<char>> = load_program("../input.txt");

    let answers: Vec<usize> = targets.into_iter().map(|t| enter_code(&t)).collect();

    let answer: usize = answers.iter().sum();
    println!("{answer}");
}

// 211068 too high
// 210392 too high
