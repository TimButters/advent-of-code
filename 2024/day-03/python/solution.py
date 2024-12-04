import re
from math import prod


def load_data(filename: str) -> str:
    with open(filename) as f:
        return "".join([line for line in f.readlines()])


def part1(program: str) -> int:
    muls = re.findall(r"mul\(\d{1,3}\,\d{1,3}\)", program)
    return sum([prod(map(int, re.findall(r"\d+", mul))) for mul in muls])


def strip_dont(program: str) -> str:
    while "don't()" in program:
        off_idx = program.index("don't()")
        if "do()" in program[off_idx:]:
            on_idx = program[off_idx:].index("do()") + off_idx
        else:
            on_idx = len(program)
        program = program[0: off_idx] + program[on_idx:]
    return program


def part2(program: str) -> int:
    stripped = strip_dont(program)
    return part1(stripped)


if __name__ == "__main__":
    filename = "input.txt"
    program = load_data(filename)
    mulsum = part1(program)
    print(f"Part 1: {mulsum}")

    print(f"Part 2: {part2(program)}")
