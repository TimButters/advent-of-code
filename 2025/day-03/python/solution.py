import sys


def load_input(filename: str) -> list[list[int]]:
    with open(filename) as f:
        return [list(map(int, line.strip())) for line in f.readlines()]


def max_location(battery: list[int]) -> tuple[int, int]:
    max_j = max(battery)
    max_idx = battery.index(max_j)
    return max_idx, max_j


def max_joltage(battery: list[int]) -> int:
    n = len(battery)
    max_idx, max_tens = max_location(battery)
    if max_idx < n-1:
        _, max_unit = max_location(battery[max_idx+1:])
    else:
        max_idx, max_tens = max_location(battery[:-1])
        _, max_unit = max_location(battery[max_idx+1:])
    return int(str(max_tens) + str(max_unit))


def part1(batteries: list[list[int]]) -> int:
    return sum([max_joltage(battery) for battery in batteries])


if __name__ == "__main__":
    filename = sys.argv[1]
    input = load_input(filename)

    print("Part 1:", part1(input))
    
