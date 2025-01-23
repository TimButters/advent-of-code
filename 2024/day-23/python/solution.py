class Node:
    def __init__(self, name: str):
        self.name = name
        self.links = []


class Graph:
    def __init__(self):
        self.nodes = {}

    def add_node(self, name: str):
        if name in self.nodes:
            return self.nodes[name]

        node = Node(name)
        self.nodes[name] = node
        return node

    def add_link(self, parent: str, child: str):
        self.nodes[child].links.append(self.nodes[parent])
        self.nodes[parent].links.append(self.nodes[child])


def load_input(filename: str) -> Graph:
    with open(filename) as f:
        pairs = [line.strip().split("-") for line in f.readlines()]

    g = Graph()
    for a, b in pairs:
        g.add_node(a)
        g.add_node(b)
        g.add_link(a, b)
    
    return g


def find_sets(g: Graph):
    sets = []
    for name, node in g.nodes.items():
        links = set(node.links)
        for subnode in node.links:
            overlap = links.intersection(subnode.links)
            if len(overlap) >= 1:
                for overlap_name in overlap:
                    if name[0] == "t" or subnode.name == "t" or overlap_name == "t":
                        s = (name, subnode.name, overlap_name.name)
                        sets.append(tuple(sorted(s)))
    return list(set(sets))


def find_largest_set(g: Graph):
    sets = []
    for name, node in g.nodes.items():
        links = set(node.links)
        overlap = set(node.links + [node])
        for link in links:
            overlap = overlap.intersection(link.links + [link])
            #print([o.name for o in overlap])
        sets.append(list(overlap))
    return sets


if __name__ == "__main__":
    filename = "test_input.txt"
    g = load_input(filename)
    nodes = find_sets(g)
    print(f"Part 1: {len(nodes)}")

    largest = find_largest_set(g)
    print(f"Part 2: {[len(l) for l in largest]}")
