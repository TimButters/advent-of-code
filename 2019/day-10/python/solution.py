from math import isclose
import sys


Point = tuple[int, int]
Grid = list[Point]

def load_input(filename: str) -> tuple[Grid, int, int]:
    with open(filename) as f:
        lines = f.readlines()
        Y = len(lines)
        X = len(lines[0])
        grid =  [(x, y) for y, line in enumerate(lines) for x, c in enumerate(line.strip()) if c != "."]
        return grid, X, Y


def _get_params(p1: Point, p2: Point) -> tuple[float, float]:
    x1, y1 = p1
    x2, y2 = p2

    m = (y2 - y1) / (x2 - x1)
    c = y1 - m * x1
    return m, c


def plot_ray(p1: Point, p2: Point) -> list[Point]:
    x1, y1 = p1
    x2, y2 = p2

    if y1 == y2:
        return [(x, y1) for x in range(min(x1, x2)+1, max(x1, x2))]

    if x1 == x2:
        return [(x1, y) for y in range(min(y1, y2)+1, max(y1, y2))]

    m, c = _get_params(p1, p2)
    step = -1 if y1 > y2 else 1

    return [(round((y-c)/m), y) for y in range(y1, y2 + step, step) if isclose((y-c)/m, round((y-c)/m))][1:-1]


def count_asteroids(point: Point, grid: Grid) -> int:
    count = 0
    points = []
    for asteroid in grid:
        if point == asteroid:
            continue
        path = plot_ray(point, asteroid)
        if not set(path).intersection(grid):
            count += 1
    return count


def part1(grid: Grid, max_x: int, max_y: int) -> int:
    return max([count_asteroids(asteroid, grid) for asteroid in grid])



if __name__ == "__main__":
    filename = sys.argv[1]
    asteroids, X, Y = load_input(filename)
    max_count = part1(asteroids, X, Y)
    print("Part 1:", max_count)
