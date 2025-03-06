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
    groups = find_sets_nodes(g)

    results = []
    node_added = True
    while node_added:
        node_added = False
        wipe_group = True if len([g for g in groups if g is not None]) > 1 else False
        for i, group in enumerate(groups):
            if group is not None:
                # print([g.name for g in group])
                group_names = set([g.name for g in group])
                min_node = min(group, key=lambda k: len(k.links))
                candidates = set([g for g in min_node.links if g.name not in group_names])
                for candidate in candidates:
                    candidate_links = set([c.name for c in candidate.links])
                    if len(candidate_links.intersection(group_names)) == len(group_names):
                        groups[i] = tuple(list(group) + [candidate])
                        node_added = True
                        wipe_group = False
                if wipe_group:
                    results.append(sorted([g.name for g in group]))
                    groups[i] = None
    return max(results, key=lambda k: len(k))
            


if __name__ == "__main__":
    filename = "input.txt"
    g = load_input(filename)
    nodes = find_sets(g)
    print(f"Part 1: {len(nodes)}")
    
    group = find_largest_set(g)
    print("Part 2:", ",".join(sorted(group)))
