use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

fn load_data(filename: &str) -> Vec<i32> {
    let file = File::open(filename).expect("Could not open file.");
    let mut reader = BufReader::new(file);

    let mut line: String = String::new();
    _ = reader.read_line(&mut line);

    let nums: Vec<i32> = line
        .trim()
        .chars()
        .map(|x| x.to_string().parse().expect("Could not parse char to int."))
        .collect();

    return nums;
}

fn count_zeros(nums: &[i32]) -> usize {
    nums.iter().filter(|n| **n == 0).count()
}

fn get_layer(layer: usize, nums: &[i32], w: usize, h: usize) -> &[i32] {
    let start: usize = layer * w * h;
    let end: usize = start + w * h;
    &nums[start..end]
}

fn find_min_layer(nums: &[i32], w: usize, h: usize) -> usize {
    let num_layers: usize = nums.len() / (w * h);
    let mut num_zeros: HashMap<usize, usize> = HashMap::<usize, usize>::new();
    for layer in 0..num_layers {
        num_zeros.insert(count_zeros(get_layer(layer, nums, w, h)), layer);
    }
    *num_zeros.get(num_zeros.keys().min().expect("I expect a min")).expect("There should be a value here.")
}

fn part1(nums: &[i32], w: usize, h: usize) -> usize {
    let layer: usize = find_min_layer(nums, w, h);
    let ones: usize = get_layer(layer, nums, w, h).iter().filter(|x| **x == 1).count();
    let twos: usize = get_layer(layer, nums, w, h).iter().filter(|x| **x == 2).count();
    return ones * twos;

}

fn main() {
    let filename = "../input.txt";
    let nums: Vec<i32> = load_data(filename);
    let p1: usize = part1(&nums, 25, 6);
    println!("{p1}");
}
