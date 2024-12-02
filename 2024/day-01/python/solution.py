from collections import Counter


def load_data(filename: str) -> tuple[list[int], list[int]]:
    with open(filename) as f:
        list1 = []
        list2 = []
        for line in f.readlines():
            v1, v2 = line.strip().split()
            list1.append(int(v1))
            list2.append(int(v2))
    return list1, list2


def part1(list1: list[int], list2: list[int]) -> int:
    return sum([abs(v1 - v2) for v1, v2 in zip(sorted(list1), sorted(list2))])


def part2(list1: list[int], list2: list[int]) -> int:
    val_counts = Counter(list2)
    return sum([v1 * val_counts[v1] for v1 in list1])


if __name__ == "__main__":
    filename = "input.txt"
    list1, list2 = load_data(filename)
    diff_sum = part1(list1, list2)
    weighted_diff = part2(list1, list2)
    print(f"Part 1: {diff_sum}")
    print(f"Part 2: {weighted_diff}")
