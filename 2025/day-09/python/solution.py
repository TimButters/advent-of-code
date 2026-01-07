import sys

from itertools import combinations
from dataclasses import dataclass


@dataclass
class Point:
    x: int
    y: int


def load_input(filename: str) -> list[Point]:
    with open(filename) as f:
        return [
            Point(int(x), int(y))
            for x, y in [line.strip().split(",") for line in f.readlines()]
        ]


def rect_area(p1: Point, p2: Point) -> int:
    return (abs(p2.x - p1.x) + 1) * (abs(p2.y - p1.y) + 1)


if __name__ == "__main__":
    filename = sys.argv[1]
    points = load_input(filename)

    print(max([rect_area(p1, p2) for p1, p2 in combinations(points, 2)]))
