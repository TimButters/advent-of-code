import sys

from functools import reduce
from operator import add, mul
from typing import Any


def load_input(filename: str) -> tuple[list[list[int]], list[Any]]:
    with open(filename) as f:
        lines = f.readlines()

    operators = []
    numbers = lines[0].strip().split()
    numbers = [[int(num)] for num in numbers]
    for line in lines[1:]:
        ns = line.strip().split()
        if ns[0] in ("+", "*"):
            operators = [add if op == "+" else mul for op in ns]
            break

        for i, n in enumerate(ns):
            numbers[i].append(int(n))

    return numbers, operators


def process_sums(numbers: list[list[int]], operators: list[Any]) -> int:
    results = []
    for op, nums in zip(operators, numbers):
        results.append(reduce(op, nums))
    return sum(results)


def load_input_p2(filename: str) -> tuple[list[list[int]], list[Any]]:
    with open(filename) as f:
        lines = f.readlines()

    M = len(lines) - 1     # Ignore operators
    N = len(lines[0]) - 1  # Ignore \n
    line_digits = [
        {i: n for i, n in enumerate(line.strip("\n")) if n != " "} for line in lines
    ]

    numbers = [[]]
    operators = []
    num = []
    i = N
    idx = 0
    while i >= 0:
        for m, ld in enumerate(line_digits):
            if m == M:
                if num:
                    numbers[idx].append(int("".join(num)))
                    num = []
                if i in ld:
                    operators.append(add if ld[i] == "+" else mul)
                    idx += 1
                    numbers.append([])
                    continue
            
            if i in ld:
                num.append(ld[i])
        i -= 1

    return numbers, operators
            


if __name__ == "__main__":
    filename = sys.argv[1]
    numbers, operators = load_input(filename)
    result = process_sums(numbers, operators)
    print("Part 1:", result)

    numbers, operators = load_input_p2(filename)
    result = process_sums(numbers, operators)
    print("Part 2:", result)

