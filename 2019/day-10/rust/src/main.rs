use is_close::default;
use std::cmp::max;
use std::cmp::min;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
struct Point {
    x: i64,
    y: i64,
}

struct LinearParams {
    intercept: f64,
    gradient: f64,
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
                x_size = line.chars().count();
            }
            y_size = rownum;
            let mut rowrocks: Vec<Point> = line
                .chars()
                .enumerate()
                .filter(|(_colnum, c)| *c != '.')
                .map(|(colnum, _)| Point {
                    x: colnum as i64,
                    y: rownum as i64,
                })
                .collect();
            rocks.append(&mut rowrocks);
        }

        return Grid {
            rocks,
            xdim: x_size,
            ydim: y_size,
        };
    }

    fn eval_asteroid(&self, p: &Point) -> usize {
        let rocks_hash: HashSet<Point> = HashSet::from_iter(self.rocks.iter().cloned());
        let mut num_visible: usize = 0;
        for asteroid in &self.rocks {
            if asteroid == p {
                continue;
            }
            let path = linear_path(&p, &asteroid);
            let blockers: usize = HashSet::from_iter(path.into_iter())
                .intersection(&rocks_hash)
                .collect::<Vec<&Point>>()
                .len();
            if blockers == 0 {
                num_visible += 1;
            }
        }
        return num_visible;
    }

    fn optimum_asteroid(&self) -> (&Point, usize) {
        let evaluations: Vec<(&Point, usize)> = self
            .rocks
            .iter()
            .map(|ast| (ast, self.eval_asteroid(ast)))
            .collect();
        return evaluations
            .into_iter()
            .max_by_key(|e| e.1)
            .expect("No maximum found.");
    }
}

fn linear_fit(p1: &Point, p2: &Point) -> LinearParams {
    let x1: f64 = p1.x as f64;
    let y1: f64 = p1.y as f64;
    let x2: f64 = p2.x as f64;
    let y2: f64 = p2.y as f64;
    let gradient = (y2 - y1) / (x2 - x1);
    let intercept = y1 - gradient * x1;
    return LinearParams {
        intercept,
        gradient,
    };
}

fn linear_path(p1: &Point, p2: &Point) -> Vec<Point> {
    if p1.x == p2.x {
        return (min(p1.y, p2.y) + 1..max(p1.y, p2.y))
            .map(|y| Point { x: p1.x, y })
            .collect();
    }

    if p1.y == p2.y {
        return (min(p1.x, p2.x) + 1..max(p1.x, p2.x))
            .map(|x| Point { x, y: p1.y })
            .collect();
    }

    let params: LinearParams = linear_fit(p1, p2);
    let mut path: Vec<Point> = Vec::<Point>::new();
    for y in min(p1.y, p2.y) + 1..max(p1.y, p2.y) {
        let x = (y as f64 - params.intercept) / params.gradient;
        if default().is_close(x, f64::round(x)) {
            path.push(Point {
                x: f64::round(x) as i64,
                y,
            });
        }
    }
    return path;
}

fn main() {
    let filename: &str = "../input.txt";
    let grid: Grid = Grid::new(filename);
    let optimum = grid.optimum_asteroid();
    println!("{optimum:?}");
}
