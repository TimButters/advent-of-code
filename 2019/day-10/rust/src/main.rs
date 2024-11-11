use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug)]
struct Point {
    x: i64,
    y: i64,
}

struct Grid {
    rocks: Vec<Point>,
    xdim: usize,
    ydim: usize,
}

impl Grid {
    fn new(filename: &str) -> Grid {
        let file = File::open(filename).expect("Could not open file.");
        let reader = BufReader::new(file);

        let mut rocks = Vec::<Point>::new();
        let mut x_size: usize = 0;
        let mut y_size: usize = 0;
        for (rownum, fileline) in reader.lines().enumerate() {
            let line: String = fileline.expect("There should be a line to read!");
            if rownum == 0 {
                x_size = line.clone().chars().count();
            }
            y_size = rownum;
            let mut rowrocks: Vec<Point> = line
                .chars()
                .enumerate()
                .filter(|(_colnum, c)| *c != '.')
                .map(|(colnum, _)| Point { x: colnum as i64, y: rownum as i64 })
                .collect();
            rocks.append(&mut rowrocks);
        }

        return Grid{ rocks, xdim: x_size, ydim: y_size };
    }
}

fn main() {
    let filename: &str = "../input.txt";
    let grid: Grid = Grid::new(filename);
    println!("{:?}", grid.xdim);
    println!("{:?}", grid.ydim);
    println!("{:?}", grid.rocks);
}
