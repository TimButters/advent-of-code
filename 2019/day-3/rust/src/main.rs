use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn load_data(filename: &str) -> Vec<Vec<(String, i32)>> {
    let file = File::open(filename).expect("File not found.");
    let reader = BufReader::new(file);

    let lines: Vec<String> = reader
        .lines()
        .map(|x: Result<String, std::io::Error>| x.expect("Could not parse line."))
        .collect();

    return lines
        .into_iter()
        .map(|x| x.split(",").map(|inst| parse_instruction(inst)).collect())
        .collect();
}

fn parse_instruction(instruction: &str) -> (String, i32) {
    let direction: String = instruction
        .chars()
        .nth(0)
        .expect("Missing first character.")
        .to_string();
    let magnitude: i32 = instruction[1..]
        .parse()
        .expect("Could not parse magnitude to int.");
    return (direction, magnitude);
}

fn map_wire(wire: &Vec<(String, i32)>) -> HashSet<(i32, i32)> {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut coords: HashSet<(i32, i32)> = HashSet::<(i32, i32)>::new();

    for (direction, magnitude) in wire {
        let xs: Vec<i32>;
        let ys: Vec<i32>;
        if direction == "U" {
            xs = vec![x; *magnitude as usize];
            ys = (y + 1..y + magnitude + 1).collect();
            y += magnitude;
        } else if direction == "D" {
            xs = vec![x; *magnitude as usize];
            ys = (y - magnitude..y).rev().collect();
            y -= magnitude;
        } else if direction == "R" {
            xs = (x + 1..x + magnitude + 1).collect();
            ys = vec![y; *magnitude as usize];
            x += magnitude;
        } else if direction == "L" {
            xs = (x - magnitude..x).rev().collect();
            ys = vec![y; *magnitude as usize];
            x -= magnitude;
        } else {
            panic!("Unrecognised direction `{direction}`.");
        }

        for (xx, yy) in xs.into_iter().zip(ys) {
            coords.insert((xx, yy));
        }
    }

    return coords;
}

fn num_steps(point: (i32, i32), w1: &HashSet<(i32, i32)>, w2: &HashSet<(i32, i32)>) -> usize {
    let index1: usize = w1.into_iter().position(|p| *p == point).unwrap();
    let index2: usize = w2.into_iter().position(|p| *p == point).unwrap();
    return index1 + index2 + 2;
}

fn l1norm(coord: &(i32, i32)) -> i32 {
    return coord.0.abs() + coord.1.abs();
}

fn main() {
    let wires: Vec<Vec<(String, i32)>> = load_data("../input.txt");
    let wireset: Vec<HashSet<(i32, i32)>> = wires
        .into_iter()
        .map(|w: Vec<(String, i32)>| map_wire(&w))
        .collect();

    let crossings = wireset[0].intersection(&wireset[1]);

    let min_crossing: i32 = crossings
        .clone()
        .into_iter()
        .map(|c: &(i32, i32)| l1norm(c))
        .min()
        .expect("No distances!");

    let min_steps: usize = crossings
        .clone()
        .into_iter()
        .map(|c: &(i32, i32)| num_steps(*c, &wireset[0], &wireset[1]))
        .min().expect("No steps!");

    println!("Part 1: {min_crossing}");
    println!("Part 2: {min_steps}");
}
