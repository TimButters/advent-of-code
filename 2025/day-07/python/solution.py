import sys

from typing import Any


Point = tuple[int, int]


class Node:
    def __init__(self, id: Any, parent=None):
        self.id = id
        self.parents: list[Node] = parent if parent is not None else []
        self.children: list[Node] = []
        self.weight: int = -1

    def __repr__(self):
        return f"Node({self.id})"

    def add_child(self, child):
        if child not in self.children:
            self.children.append(child)
        return self

    def add_parent(self, parent):
        if parent not in self.parents:
            self.parents.append(parent)
        return self


class Graph:
    def __init__(self):
        self.nodes: dict[Any, Node] = {}

    def __iter__(self):
        return iter(self.nodes)

    def __getitem__(self, idx):
        return self.nodes[idx]

    def __len__(self):
        return len(self.nodes)

    def add_node(self, id: Any, node: Node):
        self.nodes[id] = node

    def all_paths(self, start: Node, end: Node, current_path=None, paths=None) -> list[list[Node]]:
        if paths is None:
            paths = []
        if current_path is None:
            current_path = []

        current_path.append(start)

        if start == end:
            paths.append(current_path)
            return paths

        if not start.children:
            return paths

        for node in start.children:
            self.all_paths(node, end, current_path.copy(), paths)

        return paths


def load_input(filename: str) -> tuple[Point, list[Point]]:
    with open(filename) as f:
        lines = f.readlines()

    start = (-1, -1)
    splitters = []
    for y, line in enumerate(lines):
        for x, p in enumerate(line):
            if p == "S":
                start = (x, y)

            if p == "^":
                splitters.append((x, y))

    return start, splitters


def part1(start: Point, splitters: list[Point]) -> int:
    max_y = splitters[-1][1]
    max_x = max(splitters, key=lambda p: p[0])[0]

    split_points = [start]
    visited = []
    num_splits = 0
    while split_points:
        x, y = split_points.pop()
        while y <= max_y:
            y += 1
            if (x, y) in visited:
                break
            else:
                visited.append((x, y))

            if (x, y) in splitters:
                num_splits += 1

                if x < max_x and (x + 1, y) not in visited:
                    split_points.append((x + 1, y))

                if x > 0 and (x - 1, y) not in visited:
                    split_points.append((x - 1, y))

                break

    return num_splits


def build_graph(start: Point, splitters: list[Point]) -> Graph:
    max_y = splitters[-1][1] + 1

    g = Graph()
    current_splitter = Node(start)
    end_node = Node((-1, -1))
    g.add_node(start, current_splitter)
    g.add_node((-1, -1), end_node)
    split_points = [(start, current_splitter)]
    visited = []
    while split_points:
        (x, y), current_splitter = split_points.pop()
        while y <= max_y:
            y += 1
            if (x, y) not in visited:
                visited.append((x, y))

            if (x, y) in splitters:
                if (x, y) not in g:
                    g.add_node((x, y), Node((x, y)))
                else:
                    current_splitter.add_child(g.nodes[(x, y)])
                    g.nodes[(x, y)].add_parent(current_splitter)
                    break
    
                current_splitter.add_child(g.nodes[(x, y)])
                g.nodes[(x, y)].add_parent(current_splitter)
                current_splitter = g.nodes[(x, y)]

                split_points.append(((x + 1, y), current_splitter))
                split_points.append(((x - 1, y), current_splitter))

                break

            if y == max_y:
                if (x, y) in g:
                    n = g.nodes[(x, y)]
                else:
                    n = Node((x, y))
                    g.add_node((x, y), n)
                end_node.add_parent(n)
                n.add_child(end_node)
                current_splitter.add_child(n)
                n.add_parent(current_splitter)

    return g


def set_weights(g: Graph, start: Point):
    g[start].weight = 1

    ks = sorted(list(g.nodes.keys()), key=lambda k: k[1])
    ks = ks[1:] + [ks[0]]
    if ks[0] != start:
        print("Not starting at the start?")

    for k in ks[1:]:
        g[k].weight = sum([p.weight for p in g[k].parents])


if __name__ == "__main__":
    filename = sys.argv[1]
    start, splitters = load_input(filename)

    print("Part 1:", part1(start, splitters))

    g = build_graph(start, splitters)
    set_weights(g, start)
    print("Part 2:", g[(-1, -1)].weight)
