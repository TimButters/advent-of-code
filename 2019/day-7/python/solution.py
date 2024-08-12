from dataclasses import dataclass
import sys


@dataclass
class Node:
    label: str
    parent: str


class Graph:
    def __init__(self):
        self.nodes = {"COM": Node("COM", "")}

    def add_node(self, label: str, parent: str) -> None:
        self.nodes[label] = Node(label, parent)

    def all_orbits(self) -> int:
        count = 0
        for _, node in self.nodes.items():
            parent = node.parent
            while parent != "":
                parent = self.nodes[parent].parent
                count += 1
        return count


def load_data(filename: str) -> Graph:
    G = Graph()
    with open(filename) as f:
       lines = f.readlines()
       _ = [G.add_node(c.strip(), p.strip()) for p, c in [line.split(")") for line in lines]]
    return G


if __name__ == "__main__":
    filename = sys.argv[1]
    G = load_data(filename)
    print("Part 1:", G.all_orbits())
