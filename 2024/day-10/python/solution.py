Point = tuple[int, int]


class Node:
    def __init__(self, coord: Point, value: int):
        self.coord = coord
        self.value = value
        self.parents = []
        self.children = []


class Graph:
    def __init__(self):
        self.nodes = {}

    def add_node(self, coord: Point, value: int):
        if coord in self.nodes:
            raise ValueError("Node already in Graph")

        node = Node(coord, value)
        self.nodes[coord] = node
        return node

    def add_link(self, parent: Node, child: Node):
        self.nodes[child.coord].parents.append(self.nodes[parent.coord])
        self.nodes[parent.coord].children.append(self.nodes[child.coord])

    def find_children(self, coord: Point, value: int) -> list[Node]:
        x, y = coord
        candidates = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        return [
            node
            for node in self.nodes.values()
            if node.coord in candidates and node.value == value + 1
        ]

    def search(self, node: Node, finals: list[Point] = None):
        if finals is None:
            finals = []

        if node.value == 9:
            return finals.append(node.coord)

        if not node.children:
            return [None]

        for child in node.children:
            self.search(child, finals)

        return finals


def load_input(filename: str) -> list[list[int]]:
    with open(filename) as f:
        return [list(map(int, line.strip())) for line in f.readlines()]


def build_graph(mapping: list[list[int]]):
    graph = Graph()
    for level in range(9, -1, -1):
        for y, row in enumerate(mapping):
            for x, value in enumerate(row):
                if value == level:
                    node = graph.add_node((x, y), value)
                    children = graph.find_children((x, y), value)
                    _ = [graph.add_link(node, child) for child in children]
    return graph


def find_hikes(graph: Graph):
    start_points = [node for _, node in graph.nodes.items() if node.value == 0]
    hikes = []
    for point in start_points:
        score = graph.search(point)
        hikes.append((len(set(score)), len(score)))
    return hikes


if __name__ == "__main__":
    filename = "input.txt"
    mapping = load_input(filename)
    graph = build_graph(mapping)
    scores = find_hikes(graph)
    print(f"Part 1: {sum([s[0] for s in scores])}")
    print(f"Part 2: {sum([s[1] for s in scores])}")
