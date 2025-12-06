import sys


def load_input(filename: str) -> list[tuple[int, int]]:
    with open(filename) as f:
        line = f.read().strip()
    return [(int(a), int(b)) for a, b in [r.split("-") for r in line.split(",")]]


def count_invalid_part1(start: int, end: int) -> int:
    total = 0
    for id in range(start, end+1):
        sid = str(id)
        n = len(sid)
        if n % 2 == 0:
            if sid[:n//2] == sid[n//2:]:
                total += id
    return total


def count_invalid_part2(start: int, end: int) -> int:
    total = 0
    for id in range(start, end+1):
        sid = str(id)
        n = len(sid)
        subpat = [sid[0]]
        for c in sid[1:]:
            if n % len(subpat) == 0:
                m = n // len(subpat)
                if "".join(subpat * m) == sid:
                    total += id
                    break
            subpat.append(c)
            if len(subpat) > n // 2:
                break

    return total


if __name__ == "__main__":
    filename = sys.argv[1]
    ranges = load_input(filename)
    part1 = sum([count_invalid_part1(*r) for r in ranges])
    print("Part 1:", part1)

    part2 = sum([count_invalid_part2(*r) for r in ranges])
    print("Part 2:", part2)
