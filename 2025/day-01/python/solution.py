import sys


def parse_input(filename: str) -> list[tuple[str, int]]:
    with open(filename) as f:
        return [(line[0], int(line.strip()[1:])) for line in f.readlines()]


def part1(input: list[tuple[str, int]]) -> int:
    counter = 0
    pos = 50
    for dir, mag in input:
        if dir == "L":
            pos = (pos - mag) % 100
        else:
            pos = (pos + mag) % 100

        if pos == 0:
            counter += 1
    return counter


def part2(input: list[tuple[str, int]]) -> int:
    counter = 0
    pos = 50
    for dir, mag in input:
        if mag > 99:
            counter += mag // 100
            mag = mag % 100

        if dir == "L":
            if pos - mag < 1 and pos != 0:
                counter += 1
            pos = (pos - mag) % 100
        else:
            if pos + mag > 99 and pos != 0:
                counter += 1
            pos = (pos + mag) % 100

    return counter


if __name__ == "__main__":
    filename = sys.argv[1]
    input = parse_input(filename)

    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
