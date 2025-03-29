from itertools import product

Point = tuple[int, int]


def load_input(filename: str) -> list[Point]:
    with open(filename) as f:
        return [tuple(map(int, line.strip().split(","))) for line in f.readlines()]


def generate_memory_space(dim: int) -> list[Point]:
    return list(product(range(dim), range(dim)))


def get_neighbours(
    current_node: Point, maze: list[Point], visited: list[Point]
) -> list[Point]:
    x, y = current_node
    candidates = {(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)}
    return set(maze).intersection(candidates) - set(visited)


def dijkstra(maze: list[Point]) -> dict[Point, int]:
    scores = {p: 999999 for p in maze}
    unvisited = maze.copy()
    visited = []
    scores[(0, 0)] = 0
    while unvisited:
        current_node = min(unvisited, key=lambda x: scores[x])
        current_score = scores[current_node]

        for neighbour in get_neighbours(current_node, maze, visited):
            if current_score + 1 < scores[neighbour]:
                scores[neighbour] = current_score + 1

        unvisited.remove(current_node)
        visited.append(current_node)
    return scores


if __name__ == "__main__":
    filename = "input.txt"
    byte_positions = load_input(filename)
    mem_space = generate_memory_space(71)
    maze = set(mem_space) - set(byte_positions[0:1024])
    scores = dijkstra(maze)
    print(f"Part 1: {scores[(70, 70)]}")
