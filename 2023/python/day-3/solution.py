import re

from itertools import product
from functools import reduce
from operator import mul


def load_input(filename: str) -> list[str]:
    with open(filename, "r") as f:
        return [line.strip() for line in f.readlines()]


def process_lines(lines: list[str]):
    numbers = [
        (m.span(), row, int(m.group()))
        for row, line in enumerate(lines)
        for m in re.finditer(r"\d+", line)
    ]
    symbols = [
        ((m.span()[0], row), m.group())
        for row, line in enumerate(lines)
        for m in re.finditer(r"[^\d\.]", line)
    ]
    gears = [
        ((m.span()[0], row), m.group())
        for row, line in enumerate(lines)
        for m in re.finditer(r"\*", line)
    ]
    return numbers, explode_symbols(symbols), gears


def explode_symbols(symbols):
    return {
        (sx, sy)
        for (x, y), _ in symbols
        for sx, sy in product(range(x - 1, x + 2), range(y - 1, y + 2))
    }


def part1(numbers, symbols):
    return sum(
        [
            num
            for (xst, xen), y, num in numbers
            if symbols.intersection(product(range(xst, xen), [y]))
        ]
    )


def part2(numbers, gears):
    num_locations = [
        (xx, yy, num, idx)
        for idx, ((xst, xen), y, num) in enumerate(numbers)
        for xx, yy in product(range(xst, xen), [y])
    ]
    total = 0
    for gear in gears:
        gear_search = explode_symbols([gear])
        overlap = {
            idx: num for (x, y, num, idx) in num_locations if (x, y) in gear_search
        }
        if len(overlap) == 2:
            total += reduce(mul, overlap.values())
    return total


if __name__ == "__main__":
    filename = "input.txt"
    numbers, symbols, gears = process_lines(load_input(filename))
    print(part1(numbers, symbols))
    print(part2(numbers, gears))
