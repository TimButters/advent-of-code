import numpy as np


def load_input(filename: str):
    with open(filename) as f:
        return [list(map(int, line.split())) for line in f.readlines()]


def diff(line: list[int]) -> list[list[int]]:
    diffs = [line]
    current_line = line
    while any(current_line):
        current_line = list(np.diff(current_line))
        diffs.append(current_line)
    return diffs


def extrapolate(diffs: list[list[int]]):
    m = len(diffs)
    for idx in range(m-2, -1, -1):
        diffs[idx].append(diffs[idx][-1] + diffs[idx+1][-1])
    return diffs[0]


def part1(line: list[int]):
    return extrapolate(diff(line))


if __name__ == "__main__":
    filename = "input.txt"
    input = load_input(filename)
    print(sum([part1(inp)[-1] for inp in input]))
    print(sum([part1(inp[::-1])[-1] for inp in input]))
