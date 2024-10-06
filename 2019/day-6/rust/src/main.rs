use std::collections::HashMap;
use std::collections::HashSet;
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

    pub fn all_orbits(&self) -> u32 {
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

    pub fn orbital_transfers(&self, node1: &String, node2: &String) -> usize {
        let path1: HashSet<String> = self.path_to_com(node1).into_iter().collect();
        let path2: HashSet<String> = self.path_to_com(node2).into_iter().collect();
        let path_length: usize = path1.symmetric_difference(&path2).collect::<Vec<&String>>().len();
        return path_length;
    }

    fn path_to_com(&self, node: &String) -> Vec<String> {
        let mut path: Vec<String> = vec![node.clone()];
        let mut parent: String = self.nodes[node].clone();
        while parent != "" {
            path.push(parent.clone());
            parent = self.nodes[&parent].clone();
        }
        return path;
    }
}

fn main() {
    let filename = "../input.txt";
    let graph: Graph = Graph::from_file(filename);
    println!("Part 1: {}", graph.all_orbits());

    let node1: &String = &graph.nodes["YOU"];
    let node2: &String = &graph.nodes["SAN"];
    println!("Part 2: {}", graph.orbital_transfers(node1, node2));
}
