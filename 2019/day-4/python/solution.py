import sys
import re
import numpy as np


def load_input(filename: str) -> list[int]:
    with open(filename) as f:
        return list(map(int, f.read().strip().split("-")))


def check_password(password: int, part2: bool = False) -> int:
    password_split = list(map(int, list(str(password))))
    pass_diff = np.array([b - a for a, b in zip(password_split, password_split[1:])])
    if np.any(pass_diff < 0) or not np.any(pass_diff == 0):
        return 0
    if part2 and not re.match(r".*(\b|[^0])0([^0]|\b).*", "".join(map(str, pass_diff))):
        return 0
    return 1


def count_passwords(start: int, end: int, part2: bool = False) -> int:
    return sum([check_password(password, part2) for password in range(start, end + 1)])


if __name__ == "__main__":
    filename = sys.argv[1]
    pass_range = load_input(filename)
    print("Part 1:", count_passwords(pass_range[0], pass_range[1]))
    print("Part 2:", count_passwords(pass_range[0], pass_range[1], part2=True))
