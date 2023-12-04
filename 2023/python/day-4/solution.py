def load_input(filename: str) -> list[list[set[int]]]:
    with open(filename) as f:
        return [
            list(map(lambda n: set(map(int, n.strip().split())), nums))
            for nums in [line.split(":")[1].split("|") for line in f.readlines()]
        ]


def part1(cards: list[list[set[int]]]) -> int:
    return sum(
        [int(2**(len(nums.intersection(winners)) - 1)) for nums, winners in cards]
    )


def part2(cards: list[list[set[int]]]) -> int:
    counts = {k: 1 for k, _ in enumerate(cards, start=1)}
    MAX_IDX = len(cards)
    for idx, (nums, winners) in enumerate(cards, start=1):
        matches = len(nums.intersection(winners))
        for m in range(idx + 1, idx + matches + 1):
            if m <= MAX_IDX:
                counts[m] += 1 * counts[idx]
    return sum(counts.values())


if __name__ == "__main__":
    filename = "input.txt"
    cards = load_input(filename)
    print(part1(cards))
    print(part2(cards))
