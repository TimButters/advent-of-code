import re


def part1(filename: str) -> int:
    with open(filename) as f:
        return sum([int(r[0] + r[-1]) for r in [re.findall(r"\d", line) for line in f.readlines()]])


def part2(filename: str) -> int:
    d = {
        "one": "1",
        "two": "2",
        "three": "3",
        "four": "4",
        "five": "5",
        "six": "6",
        "seven": "7",
        "eight": "8",
        "nine": "9",
    }
    with open(filename) as f:
        return sum(
            [
                _translate(re.findall(rf"(?=(\d|{'|'.join(d.keys())}))", line), d)
                for line in f.readlines()
            ]
        )


def _translate(input, d):
    n = [d[i] if i in d else i for i in input]
    return int(n[0] + n[-1])


if __name__ == "__main__":
    filename = "input.txt"
    print(part1(filename))
    print(part2(filename))
