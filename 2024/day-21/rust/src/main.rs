use factorial::Factorial;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, Clone)]
struct Point {
    x: i64,
    y: i64,
}

struct Arm {
    pos: Point,
    coords: HashMap<char, Point>,
}

impl Arm {
    fn new() -> Arm {
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
        let start_pos = coord_map[&'A'].clone();
        return Arm {
            pos: start_pos,
            coords: coord_map,
        };
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

fn main() {
    let target = load_program("../test_input.txt");

    let mut arm = Arm::new();

    for c in target {
        let combinations = arm.move_arm(c);
        println!("{c}\t{combinations}");
    }
}
