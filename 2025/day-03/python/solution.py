import sys


def load_input(filename: str) -> list[list[int]]:
    with open(filename) as f:
        return [list(map(int, line.strip())) for line in f.readlines()]


if __name__ == "__main__":
    filename = sys.argv[1]
    input = load_input(filename)
    print(input)
