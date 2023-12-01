import re


def read_input(filename: str) -> list[list[str]]:
    with open(filename) as f:
        return [re.findall(r"\d", line) for line in f.readlines()]


def read_input2(filename: str) -> list[list[str]]:
    with open(filename) as f:
        return [re.findall(r"\d", replace_nums(line)) for line in f.readlines()]


def sum_nums(input: list[list[str]]):
    return sum(
        [
            int(line[0] + line[-1]) if len(line) > 1 else int(line[0] * 2)
            for line in input
        ]
    )


def replace_nums(line: str) -> str:
    line = re.sub(r"one", "o1e", line)
    line = re.sub(r"two", "t2o", line)
    line = re.sub(r"three", "t3e", line)
    line = re.sub(r"four", "f4r", line)
    line = re.sub(r"five", "f5e", line)
    line = re.sub(r"six", "s6x", line)
    line = re.sub(r"seven", "s7n", line)
    line = re.sub(r"eight", "e8t", line)
    line = re.sub(r"nine", "n9e", line)
    return line


if __name__ == "__main__":
    filename = "input.txt"
    input = read_input(filename)
    print(sum_nums(input))

    input2 = read_input2(filename)
    print(sum_nums(input2))
