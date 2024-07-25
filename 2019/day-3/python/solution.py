import sys


Point = tuple[int, int]
Path = list[Point]


def load_data(filename: str) -> list[list[str]]:
    with open(filename) as f:
        return [
            [w.strip() for w in wire.split(",") if w]
            for wire in [line for line in f.readlines() if line]
        ]


def l1norm(coord: Point) -> int:
    return abs(coord[0]) + abs(coord[1])


def map_wire(wire: list[str]) -> list[Path]:
    x = 0
    y = 0
    coords = []
    for instruction in wire:
        direction = instruction[0]
        magnitude = int(instruction[1:])
        if direction == "U":
            ys = range(y + 1, y + magnitude + 1)
            xs = [x] * len(ys)
            y += magnitude
        elif direction == "D":
            ys = range(y - 1, y - magnitude - 1, -1)
            xs = [x] * len(ys)
            y -= magnitude
        elif direction == "L":
            xs = range(x - 1, x - magnitude - 1, -1)
            ys = [y] * len(xs)
            x -= magnitude
        elif direction == "R":
            xs = range(x + 1, x + magnitude + 1)
            ys = [y] * len(xs)
            x += magnitude
        else:
            raise ValueError("Unrecognised direction", direction)
        coords += [(xx, yy) for xx, yy in zip(xs, ys)]
    return coords


def crossings(wires: list[list[str]]) -> tuple[set[Point], Path, Path]:
    wire1, wire2 = [map_wire(wire) for wire in wires]
    crossings = set(wire1).intersection(set(wire2))
    return crossings, wire1, wire2


def part1(wires: list[list[str]]) -> int:
    coords, _, _ = crossings(wires)
    return min(l1norm(coord) for coord in coords)


def step_norm(point: Point, path1: Path, path2: Path) -> int:
    return path1.index(point) + path2.index(point) + 2


def part2(wires: list[list[str]]) -> int:
    coords, wire1, wire2 = crossings(wires)
    return min(step_norm(coord, wire1, wire2) for coord in coords)


if __name__ == "__main__":
    filename = sys.argv[1]
    wires = load_data(filename)
    print("Part 1:", part1(wires))
    print("Part 2:", part2(wires))
