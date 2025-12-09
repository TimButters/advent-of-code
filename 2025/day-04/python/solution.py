import sys

Point = tuple[int, int]


def load_input(filename: str) -> set[Point]:
    with open(filename) as f:
        lines = f.readlines()

    rolls = []
    for j, line in enumerate(lines):
        for i, c in enumerate(line):
            if c == "@":
                rolls.append((i, j))
    return set(rolls)


def check_roll(point: Point, rolls: set[Point]) -> bool:
    x, y = point
    field = {
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1),
        (x + 1, y + 1),
        (x - 1, y - 1),
        (x + 1, y - 1),
        (x - 1, y + 1),
    }
    score = len(field.intersection(rolls))
    return True if score < 4 else False


def part1(rolls: set[Point]) -> int:
    return sum([1 if check_roll(roll, rolls) else 0 for roll in rolls])


def part2(rolls: set[Point]) -> int:
    num_rolls = len(rolls)
    to_remove = []
    while True:
        for roll in rolls:
            if check_roll(roll, rolls):
                to_remove.append(roll)
        
        if not to_remove:
            break
        
        rolls = rolls ^ set(to_remove)
        to_remove = []

    return num_rolls - len(rolls)


if __name__ == "__main__":
    filename = sys.argv[1]
    rolls = load_input(filename)

    print("Part 1:", part1(rolls))
    print("Part 2:", part2(rolls))
