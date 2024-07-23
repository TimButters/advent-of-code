import sys


def load_data(filename: str) -> list[list[str]]:
    with open(filename) as f:
        return [[w.strip() for w in wire.split(",") if w] for wire in [line for line in f.readlines() if line]]


def l1norm(coord: tuple[int, int]) -> int:
    return abs(coord[0]) + abs(coord[1])


def map_wire(wire: list[str]):
    x = 0
    y = 0
    coords = []
    for instruction in wire:
        direction = instruction[0]
        magnitude = int(instruction[1:])
        if direction == "U":
            ys = range(y, y + magnitude + 1)
            xs = [x] * len(ys)
            y += magnitude
        elif direction == "D":
            ys = range(y, y - magnitude - 1, -1)
            xs = [x] * len(ys)
            y -= magnitude
        elif direction == "L":
            xs = range(x, x - magnitude - 1, -1)
            ys = [y] * len(xs)
            x -= magnitude
        elif direction == "R":
            xs = range(x, x + magnitude + 1)
            ys = [y] * len(xs)
            x += magnitude
        else:
            raise ValueError("Unrecognised direction", direction)
        coords += [(xx, yy) for xx, yy in zip(xs, ys)]
    return coords


def part1(wires: list[list[str]]) -> int:
    wire1, wire2 = [map_wire(wire) for wire in wires]
    crossings = set(wire1).intersection(set(wire2))
    crossings.remove((0, 0))
    return min(l1norm(coord) for coord in crossings)


if __name__ == "__main__":
    filename = sys.argv[1]
    wires = load_data(filename)
    print("Part 1:", part1(wires))
