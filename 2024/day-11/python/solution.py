def load_input(filename: str):
    with open(filename) as f:
        return f.read().strip().split(" ")


def rules(stone: str) -> list[str]:
    if stone == "0":
        return ["1"]

    if len(stone) % 2 == 0:
        N = len(stone)
        left = stone[0 : N // 2]
        right = stone[N // 2 :]
        return [str(int(left)), str(int(right))]

    return [str(int(stone) * 2024)]


def blink(stones: list[str]) -> list[str]:
    return [s for stone in stones for s in rules(stone)]


if __name__ == "__main__":
    filename = "input.txt"
    stones = load_input(filename)
    N = len(stones)
    for _ in range(25):
        stones = blink(stones)
        N = len(stones)
    print(f"Part 1: {len(stones)}\n")

    for _ in range(25):
        stones = blink(stones)
        print(len(stones), int(N * 1.52))
        N = int(N * 1.52)
