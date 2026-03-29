import sys

from itertools import combinations
from dataclasses import dataclass
from line_profiler import profile


@dataclass
class Point:
    x: int
    y: int

    def __hash__(self):
        return hash((self.x, self.y))


def load_input(filename: str) -> list[Point]:
    with open(filename) as f:
        return [
            Point(int(x), int(y))
            for x, y in [line.strip().split(",") for line in f.readlines()]
        ]


def rect_area(p1: Point, p2: Point) -> int:
    return (abs(p2.x - p1.x) + 1) * (abs(p2.y - p1.y) + 1)


@profile
def shape_boundary(points: list[Point]) -> list[Point]:
    min_x = min(points, key=lambda p: p.x).x
    max_x = max(points, key=lambda p: p.x).x
    min_y = min(points, key=lambda p: p.y).y
    max_y = max(points, key=lambda p: p.y).y

    new_points = points.copy()

    for col in range(min_x, max_x + 1):
        subset = [p for p in points if p.x == col]
        if subset:
            row_min_y = min(subset, key=lambda p: p.y).y
            row_max_y = max(subset, key=lambda p: p.y).y
            new_points += [Point(col, y) for y in range(row_min_y, row_max_y)]

    for row in range(min_y, max_y + 1):
        subset = [p for p in points if p.y == row]
        if subset:
            row_min_x = min(subset, key=lambda p: p.x).x
            row_max_x = max(subset, key=lambda p: p.x).x
            new_points += [Point(x, row) for x in range(row_min_x, row_max_x)]

    return list(set(new_points))


def fill_shape(points: list[Point]) -> list[Point]:
    max_x = max(points, key=lambda p: p.x).x
    max_y = max(points, key=lambda p: p.y).y

    new_points = points.copy()

    for y in range(max_y):
        for x in range(max_x):
            if Point(x, y) in points:
                continue

            x_col_u = [p for p in points if p.y == y and p.x > x]
            y_col_u = [p for p in points if p.x == x and p.y > y]
            x_col_d = [p for p in points if p.y == y and p.x < x]
            y_col_d = [p for p in points if p.x == x and p.y < y]

            if not x_col_u or not y_col_u or not x_col_d or not y_col_d:
                pass
            elif len(x_col_u) % 2 != 0 and len(y_col_u) % 2 != 0:
                new_points.append(Point(x, y))
            elif min(len(x_col_u), len(y_col_u)) % 2 != 0:
                new_points.append(Point(x, y))
            else:
                pass
    return new_points


@profile
def in_shape(point: Point, points: list[Point]) -> bool:
    if point in points:
        return True

    x_col_u = [p for p in points if p.y == point.y and p.x > point.x]
    y_col_u = [p for p in points if p.x == point.x and p.y > point.y]
    x_col_d = [p for p in points if p.y == point.y and p.x < point.x]
    y_col_d = [p for p in points if p.x == point.x and p.y < point.y]

    if not x_col_u or not y_col_u or not x_col_d or not y_col_d:
        return False
    elif len(x_col_u) % 2 != 0 and len(y_col_u) % 2 != 0:
        return True
    elif min(len(x_col_u), len(y_col_u)) % 2 != 0:
        return True
    else:
        return False


@profile
def rect_area_mod(p1: Point, p2: Point, points: list[Point]) -> int:
    other_a = Point(p1.x, p2.y)
    other_b = Point(p2.x, p1.y)
    if not in_shape(other_a, points) or not in_shape(other_b, points):
        return 0
    else:
        return (abs(p2.x - p1.x) + 1) * (abs(p2.y - p1.y) + 1)


def part2(areas: list[tuple[Point, Point, int]], points: list[Point]) -> int:
    boundary = set(shape_boundary(points))
    for p1, p2, area in areas:
        if rect_area_mod(p1, p2, boundary) > 0:
            return area
    return -1


if __name__ == "__main__":
    filename = sys.argv[1]
    points = load_input(filename)

    max_area = max([rect_area(p1, p2) for p1, p2 in combinations(points, 2)])
    print("Part 1:", max_area)

    #areas = sorted(
    #    [(p1, p2, rect_area(p1, p2)) for p1, p2 in combinations(points, 2)],
    #    key=lambda x: x[2],
    #    reverse=True,
    #)

    #max_area = part2(areas, points)
    #print("Part 2:", max_area)
