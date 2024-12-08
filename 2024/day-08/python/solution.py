from itertools import combinations


Point = tuple[int, int]


def load_input(filename: str):
    map = {}
    with open(filename) as f:
        for y, line in enumerate(f.readlines()):
            for x, c in enumerate(line.strip()):
                if c == ".":
                    continue

                if c not in map:
                    map[c] = [(x, y)]
                else:
                    map[c].append((x, y))
    return map, x + 1, y + 1


def generate_nodes(a: Point, b: Point):
    xa, ya = a
    xb, yb = b
    xdiff = abs(xa - xb)
    ydiff = abs(ya - yb)
    new_xa = xa + xdiff if xa > xb else xa - xdiff
    new_ya = ya + ydiff if ya > yb else ya - ydiff
    new_xb = xb + xdiff if xb > xa else xb - xdiff
    new_yb = yb + ydiff if yb > ya else yb - ydiff
    return (new_xa, new_ya), (new_xb, new_yb)


def get_all_nodes(points: list[Point], part2=False):
    pairs = [
        generate_nodes_repeat(p1, p2, max_x, max_y) if part2 else generate_nodes(p1, p2)
        for p1, p2 in combinations(points, 2)
    ]
    return [a for b in pairs for a in b]


def find_nodes(map, max_x, max_y, part2=False) -> set[Point]:
    nodes = set()
    for char, points in map.items():
        nodes |= {
            node
            for node in get_all_nodes(points, part2)
            if node[0] >= 0 and node[0] < max_x and node[1] >= 0 and node[1] < max_y
        }
    return nodes


def repeater(p: Point, xdiff: int, ydiff: int, max_x: int, max_y: int):
    x, y = p
    new_points = []
    new_x = x + xdiff
    new_y = y + ydiff
    while new_x >= 0 and new_x < max_x and new_y >= 0 and new_y < max_y:
        new_points.append((new_x, new_y))
        new_x += xdiff
        new_y += ydiff
    return new_points


def generate_nodes_repeat(a: Point, b: Point, max_x, max_y):
    xa, ya = a
    xb, yb = b
    xdiff = abs(xa - xb)
    ydiff = abs(ya - yb)

    xa_diff = xdiff if xa > xb else -xdiff
    ya_diff = ydiff if ya > yb else -ydiff
    xb_diff = xdiff if xb > xa else -xdiff
    yb_diff = ydiff if yb > ya else -ydiff

    new_as = repeater(a, xa_diff, ya_diff, max_x, max_y)
    new_bs = repeater(b, xb_diff, yb_diff, max_x, max_y)

    return new_as + new_bs


if __name__ == "__main__":
    filename = "input.txt"
    map, max_x, max_y = load_input(filename)
    nodes = find_nodes(map, max_x, max_y)
    print(f"Part 1: {len(nodes)}")

    nodes = find_nodes(map, max_x, max_y, True)
    antenna = {p for _, points in map.items() for p in points}
    print(f"Part 2: {len(nodes | antenna)}")
