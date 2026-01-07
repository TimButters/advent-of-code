import sys

from math import sqrt
from dataclasses import dataclass
from itertools import combinations
from functools import reduce


@dataclass
class Point:
    x: int
    y: int
    z: int

    def __hash__(self):
        return hash((self.x, self.y, self.z))

    @staticmethod
    def distance(left, right):
        return sqrt(
            (right.x - left.x) ** 2 + (right.y - left.y) ** 2 + (right.z - left.z) ** 2
        )


def load_input(filename: str) -> list[Point]:
    with open(filename) as f:
        return [Point(*map(int, line.strip().split(","))) for line in f.readlines()]


def points_in_circuit(
    p1: Point, p2: Point, circuits: list[list[Point]]
) -> tuple[int, int]:
    p1_idx = -1
    p2_idx = -1
    for idx, circuit in enumerate(circuits):
        if p1 in circuit:
            p1_idx = idx
        if p2 in circuit:
            p2_idx = idx
    return p1_idx, p2_idx


def part1(boxes, N=1000):
    points = sorted(
        [(p1, p2, Point.distance(p1, p2)) for p1, p2 in combinations(boxes, 2)],
        key=lambda x: x[2],
    )
    circuits = [[points[0][0], points[0][1]]]
    count = 0
    for p1, p2, dist in points[1:]:
        p1_idx, p2_idx = points_in_circuit(p1, p2, circuits)
        if p1_idx < 0 and p2_idx < 0:
            circuits.append([p1, p2])
        elif p1_idx < 0 or p2_idx < 0:
            idx = p1_idx if p2_idx < 0 else p2_idx
            circuits[idx] += [p1, p2]
        elif p1_idx == p2_idx:
            pass
        else:
            circuits[p1_idx] += [p2] + circuits[p2_idx]
            circuits[p2_idx] = []

        count += 1
        if count == N - 1:
            break

    results = sorted(
        [len(set(circuit)) for circuit in circuits if circuit], reverse=True
    )[0:3]
    return reduce(lambda x, y: x * y, results)


def part2(boxes):
    points = sorted(
        [(p1, p2, Point.distance(p1, p2)) for p1, p2 in combinations(boxes, 2)],
        key=lambda x: x[2],
    )
    circuits = [[points[0][0], points[0][1]]]

    for p1, p2, dist in points[1:]:
        p1_idx, p2_idx = points_in_circuit(p1, p2, circuits)
        if p1_idx < 0 and p2_idx < 0:
            circuits.append([p1, p2])
        elif p1_idx < 0 or p2_idx < 0:
            idx = p1_idx if p2_idx < 0 else p2_idx
            circuits[idx] += [p1, p2]
        elif p1_idx == p2_idx:
            pass
        else:
            circuits[p1_idx] += [p2] + circuits[p2_idx]
            circuits[p2_idx] = []

        populated_circuits = [circuit for circuit in circuits if circuit]
        if len(populated_circuits) == 1 and len(set(populated_circuits[0])) == len(boxes):
            return p1.x * p2.x
    
    return None



if __name__ == "__main__":
    filename = sys.argv[1]
    boxes = load_input(filename)

    results = part1(boxes)
    print("Part 1:", results)

    print("Part 2:", part2(boxes))

