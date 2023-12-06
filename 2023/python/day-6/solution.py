from functools import reduce
from operator import mul


def load_input(filename: str):
    with open(filename) as f:
        return [list(map(int, line.split(":")[1].strip().split())) for line in f.readlines()]


def find_winners(times, distances):
    return reduce(mul, [sum([1 for tp in range(1, T) if (T-tp)*tp > D]) for T, D in zip(times, distances)])


def part2(times, distances):
    T = int("".join(map(str, times)))
    D = int("".join(map(str, distances)))
    return sum([1 for tp in range(1, T) if (T-tp)*tp > D])


if __name__ == "__main__":
    filename = "input.txt"
    races = load_input(filename)
    winners = find_winners(races[0], races[1])
    print(winners)
    print(part2(races[0], races[1]))
