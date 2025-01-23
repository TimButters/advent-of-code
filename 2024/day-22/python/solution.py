def load_input(filename: str) -> list[int]:
    with open(filename) as f:
        return [int(line.strip()) for line in f.readlines()]


def next_number(n: int) -> int:
    n = (n ^ (n * 64)) % 16777216
    n = (n ^ (n // 32)) % 16777216
    n = (n ^ (n * 2048)) % 16777216
    return n


def n_secret_numbers(num: int, n: int) -> int:
    for _ in range(n):
        num = next_number(num)
    return num


if __name__ == "__main__":
    filename = "input.txt"
    starts = load_input(filename)
    print(sum([n_secret_numbers(n, 2000) for n in starts]))
