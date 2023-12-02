from functools import reduce
from operator import mul


def load_input(filename: str):
    with open(filename, "r") as f:
        return [parse_line(line) for line in f.readlines()]


def parse_line(line: str) -> tuple[int, dict[str, int]]:
    left, right = line.split(":")
    idx = int(left.split()[1])
    games = right.split(";")
    d = {}
    for game in games:
        colours = game.split(",")
        for colour in colours:
            quant, col = colour.split()
            quant = int(quant)
            d[col] = quant if col not in d or quant > d[col] else d[col]
    return (idx, d)


if __name__ == "__main__":
    filename = "input.txt"
    games = load_input(filename)

    cube_sum = 0
    for idx, game in games:
        if "blue" in game and game["blue"] > 14:
            continue
        if "red" in game and game["red"] > 12:
            continue
        if "green" in game and game["green"] > 13:
            continue
        cube_sum += idx
    print(cube_sum)

    print(sum([reduce(mul, game.values()) for _, game in games]))
