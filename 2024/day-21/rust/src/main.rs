use factorial::Factorial;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, Clone, Copy)]
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
            ('A', Point { x: 2, y: 3 }),
            ('0', Point { x: 1, y: 3 }),
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
            ('A', Point { x: 2, y: 0 }),
            ('^', Point { x: 1, y: 0 }),
            ('<', Point { x: 0, y: 1 }),
            ('v', Point { x: 1, y: 1 }),
            ('>', Point { x: 2, y: 1 }),
        ]);
        return Arm::new(coord_map);
    }

    fn move_arm(&mut self, target: char) -> u64 {
        let destination: &Point = &self.coords[&target];
        let x_diff: u64 = (self.pos.x - destination.x)
            .abs()
            .try_into()
            .expect("Could not convert to unsigned int.");
        let y_diff: u64 = (self.pos.y - destination.y)
            .abs()
            .try_into()
            .expect("Could not convert to unsigned int.");
        let combinations: u64 =
            (x_diff + y_diff).factorial() / (x_diff.factorial() * y_diff.factorial());

        self.pos = destination.clone();
        return combinations;
    }
}

fn load_program(filename: &str) -> Vec<char> {
    let file = File::open(filename).expect("File not found.");
    let mut reader = BufReader::new(file);

    let mut code: String = String::new();
    reader
        .read_line(&mut code)
        .expect("Error reading line from file.");

    return code.trim().chars().collect();
}

fn arm_moves_arm(arm: &mut Arm, target: &char) -> Vec<char> {
    let target_coord = arm.coords[&target];
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

    let mut sequence: Vec<char> = vec![];
    for _ in 0..x_change.abs() {
        sequence.push(x_button);
    }
    for _ in 0..y_change.abs() {
        sequence.push(y_button);
    }
    sequence.push('A');
    return sequence;
}

fn process_movement(arm: &mut Arm, target: &Vec<char>) {
    let mut seqs: Vec<Vec<char>> = vec![];
    for c in target {
        let seq: Vec<char> = arm_moves_arm(arm, &c);
        seqs.push(seq);
    }
    let s: String = seqs.into_iter().flatten().collect::<Vec<char>>().iter().collect();
    println!("{s}");
}

fn main() {
    let target = load_program("../test_input.txt");

    let mut numpad = Arm::new_numpad();
    let dirpad1 = Arm::new_dirpad();


    process_movement(&mut numpad, &target);

}
