use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;

pub struct Graph {
    nodes: HashMap<String, String>,
}

impl Graph {
    fn new() -> Graph {
        let mut graph: Graph = Graph {
            nodes: HashMap::<String, String>::new(),
        };
        graph.nodes.insert("COM".to_string(), "".to_string());
        return graph;
    }

    fn from_file(filename: &str) -> Graph {
        let file = File::open(filename).expect("Could not open file.");
        let reader = BufReader::new(file);

        let mut graph: Graph = Graph::new();

        for line in reader.lines() {
            if let Some((parent, node)) = line.expect("Could not read line.").split_once(")") {
                graph.add_node(node.to_string(), parent.to_string());
            } else {
                println!("No node added for line");
            }
        }

        return graph;
    }

    pub fn add_node(&mut self, label: String, parent: String) -> () {
        self.nodes.insert(label, parent);
    }

    pub fn all_orbits(self) -> u32 {
        let mut count: u32 = 0;
        for (_, parent) in &self.nodes {
            let mut current_parent: String = parent.clone();
            while current_parent != "" {
                current_parent = self.nodes[&current_parent].clone();
                count += 1;
            }
        }
        return count;
    }
}

fn main() {
    let filename = "../input.txt";
    let graph: Graph = Graph::from_file(filename);
    println!("Part 1: {}", graph.all_orbits());
}
