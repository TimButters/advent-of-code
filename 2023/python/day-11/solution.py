from itertools import combinations


def load_input(filename: str):
    with open(filename) as f:
        return [list(line.strip()) for line in f.readlines() if line]


def parse_input(input: list[list[str]]) -> list[tuple[int, int]]:
    coords = []
    for y, line in enumerate(input):
        coords.append([(x, y) for x, c in enumerate(line) if c == "#"])
    return [(x, y) for row in coords for x, y in row]


def expand_universe(universe, factor=2):
    xs = {x for x, _ in universe}
    ys = {y for _, y in universe}
    empty_xs = set(range(0, max(xs))) - xs
    empty_ys = set(range(0, max(ys))) - ys
    return [
        (
            x + sum([factor-1 for dx in empty_xs if x > dx]),
            y + sum([factor-1 for dy in empty_ys if y > dy]),
        )
        for x, y in universe
    ]


def one_norm(coord1, coord2):
    x1, y1 = coord1
    x2, y2 = coord2
    return abs(x2 - x1) + abs(y2 - y1)


def sum_shortest_paths(universe):
    return sum(
        [
            one_norm(coord1, coord2)
            for coord1, coord2 in combinations(expanded_universe, r=2)
        ]
    )


if __name__ == "__main__":
    filename = "input.txt"
    input = load_input(filename)
    universe = parse_input(input)
    expanded_universe = expand_universe(universe)
    print(sum_shortest_paths(expanded_universe))

    expanded_universe = expand_universe(universe, factor=1000000)
    print(sum_shortest_paths(expanded_universe))
