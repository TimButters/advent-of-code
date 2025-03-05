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


def find_sets_nodes(g: Graph):
    sets = []
    for name, node in g.nodes.items():
        links = set(node.links)
        for subnode in node.links:
            overlap = links.intersection(subnode.links)
            if len(overlap) >= 1:
                for overlap_name in overlap:
                    s = (node, subnode, overlap_name)
                    sets.append(tuple(s))
    return list(set(sets))


def find_largest_set(g: Graph):
    group_ranks = dict()
    threes = find_sets_nodes(g)
    for nodes in threes:
        for node in nodes:
            print(sorted([link.name for link in node.links]))

        rank = set(nodes[0].links).intersection(nodes[1].links).intersection(nodes[2].links)
        print([r.name for r in rank])
        rank = len(rank)

        if rank > 3:
            if rank in group_ranks:
                group_ranks[rank].append(nodes)
            else:
                group_ranks[rank] = [nodes]
        break
    return group_ranks


if __name__ == "__main__":
    filename = "input.txt"
    g = load_input(filename)
    nodes = find_sets(g)
    print(f"Part 1: {len(nodes)}")

    group_ranks = find_largest_set(g)
    for k, v in group_ranks.items():
        print(k, len(v))
