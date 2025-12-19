import sys


Point = tuple[int, int]


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
    max_x = max(splitters, key = lambda p: p[0])[0]

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


if __name__ == "__main__":
    filename = sys.argv[1]
    start, splitters = load_input(filename)

    print("Part 1:", part1(start, splitters))
