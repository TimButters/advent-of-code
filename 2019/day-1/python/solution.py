import sys


def load_data(filename: str) -> list[int]:
    with open(filename) as f:
        return [int(line) for line in f.readlines() if line]


def required_fuel(mass: int) -> int:
    return (mass // 3) - 2


def part1(modules: list[int]) -> int:
    return sum([required_fuel(module) for module in modules])


def part2(modules: list[int]) -> int:
    fuel = 0
    for module in modules:
        fuel_iteration = required_fuel(module)
        while fuel_iteration > 0:
            fuel += fuel_iteration
            fuel_iteration = required_fuel(fuel_iteration)
    return fuel


if __name__ == "__main__":
    filename = "../input.txt" if len(sys.argv) < 2 else sys.argv[1]
    data = load_data(filename)
    print("Part 1:", part1(data))
    print("Part 2:", part2(data))
