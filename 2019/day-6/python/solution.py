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

    def path_to_com(self, node: str) -> list[str]:
        n = self.nodes[node]
        parents = [node]
        while n.parent != "":
            parents.append(n.parent)
            n = self.nodes[n.parent]
        return parents

    def orbital_transfers(self, node1: str, node2: str) -> int:
        p1 = set(self.path_to_com(node1))
        p2 = set(self.path_to_com(node2))
        return len(p1.symmetric_difference(p2))


def load_data(filename: str) -> Graph:
    G = Graph()
    with open(filename) as f:
        lines = f.readlines()
        _ = [
            G.add_node(c.strip(), p.strip())
            for p, c in [line.split(")") for line in lines]
        ]
    return G


if __name__ == "__main__":
    filename = sys.argv[1]
    G = load_data(filename)
    print("Part 1:", G.all_orbits())

    n1 = G.nodes["YOU"].parent
    n2 = G.nodes["SAN"].parent
    print("part 2:", G.orbital_transfers(n1, n2))
