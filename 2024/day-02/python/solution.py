Record = list[int]


def load_data(filename: str) -> list[Record]:
    with open(filename) as f:
        return [list(map(int, line.split())) for line in f.readlines()]


def is_record_safe(record: Record) -> bool:
    diffs = [a-b for a, b in zip(record[1:], record)]
    is_same_direction = not (min(diffs) < 0 and max(diffs) > 0)
    is_within_limits = max(map(abs, diffs)) < 4
    is_all_changing = 0 not in diffs
    return is_same_direction and is_within_limits and is_all_changing


def part1(records: list[Record]) -> int:
    return sum([is_record_safe(record) for record in records])


def generate_with_removal(record: Record):
    return [record[0:i] + record[i+1:] for i in range(len(record))]


def check_with_removal(record: Record) -> bool:
    return any([is_record_safe(r) for r in generate_with_removal(record)])


def part2(records: list[Record]) -> int:
    return sum([check_with_removal(record) for record in records])


if __name__ == "__main__":
    filename = "input.txt"
    records = load_data(filename)
    print(f"Part 1: {part1(records)}")
    print(f"Part 2: {part2(records)}")
