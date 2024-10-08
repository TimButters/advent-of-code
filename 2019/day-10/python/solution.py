from math import isclose, sqrt, atan2, pi
from itertools import groupby, cycle
import sys


Point = tuple[int, int]
Grid = list[Point]
VPoint = tuple[int, int, int, int]
VFPoint = tuple[float, float, int, int]
VFGrid = list[VFPoint]


def load_input(filename: str) -> tuple[Grid, int, int]:
    with open(filename) as f:
        lines = f.readlines()
        Y = len(lines)
        X = len(lines[0])
        grid = [
            (x, y)
            for y, line in enumerate(lines)
            for x, c in enumerate(line.strip())
            if c != "."
        ]
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
        return [(x, y1) for x in range(min(x1, x2) + 1, max(x1, x2))]

    if x1 == x2:
        return [(x1, y) for y in range(min(y1, y2) + 1, max(y1, y2))]

    m, c = _get_params(p1, p2)
    step = -1 if y1 > y2 else 1

    return [
        (round((y - c) / m), y)
        for y in range(y1, y2 + step, step)
        if isclose((y - c) / m, round((y - c) / m))
    ][1:-1]


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


def part1(grid: Grid, max_x: int, max_y: int) -> tuple[Point, int]:
    return max(
        [(asteroid, count_asteroids(asteroid, grid)) for asteroid in grid],
        key=lambda x: x[1],
    )


def cart2pol(point: VPoint) -> VFPoint:
    x, y, X, Y = point
    r = sqrt(x**2 + y**2)
    theta = atan2(y, x) % (2 * pi)
    return r, theta, X, Y


def rotate_polar_grid(polar_grid) -> VFGrid:
    def _rotate(t: float) -> float:
        converted_t = t % (2 * pi)  # Move to 0 - 2PI range
        converted_t = (converted_t - ((3 / 4 * 2 * pi))) % (
            2 * pi
        )  # Make straight up 0 rads
        return converted_t

    return [(r, _rotate(t), x, y) for r, t, x, y in polar_grid]


def grid2polar(grid: Grid, point: Point = (0, 0)) -> VFGrid:
    px, py = point
    shifted_grid = [(x - px, y - py, x, y) for x, y in grid]
    polar_grid = [cart2pol(p) for p in shifted_grid]
    return rotate_polar_grid(polar_grid)


if __name__ == "__main__":
    filename = sys.argv[1]
    asteroids, X, Y = load_input(filename)
    station, max_count = part1(asteroids, X, Y)
    print("Part 1:", max_count)

    polar = [
        (r, theta, x, y) for r, theta, x, y in grid2polar(asteroids, station) if r != 0
    ]
    polar.sort(key=lambda x: (x[1], x[0]))
    targets = [
        (angle, list(grp)[::-1]) for angle, grp in groupby(polar, key=lambda x: x[1])
    ]
    c = cycle(targets)

    destroyed = []
    while len(destroyed) < 200:
        theta, coords = next(c)
        if coords:
            destroyed.append(coords.pop())

    _, _, x, y = destroyed[-1]
    print("Part 2:", x * 100 + y)
