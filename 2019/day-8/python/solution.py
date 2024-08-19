import sys


def load_data(filename: str):
    with open(filename) as f:
        return [int(d) for d in f.read().strip()]


def find_layer(img, W, H) -> int:
    step = W * H
    counts = [
        len([d for d in img[i : i + step] if d == 0])
        for n, i in enumerate(range(0, len(img), step))
    ]
    return counts.index(min(counts))


def get_layer(img: list[int], layer: int, W: int, H: int) -> list[int]:
    idx = layer * W * H
    return img[idx : idx + W * H]


def part1(img: list[int], W: int, H: int) -> int:
    min_zero_layer = find_layer(img, W, H)
    layer = get_layer(img, min_zero_layer, W, H)
    ones = len([d for d in layer if d == 1])
    twos = len([d for d in layer if d == 2])
    return ones * twos


def part2(img: list[int], W: int, H: int) -> list[int]:
    step = W * H
    visible = [-1] * step
    for i in range(0, len(img), step):
        layer = img[i : i + step]
        for j, d in enumerate(layer):
            if d < 2 and visible[j] < 0:
                visible[j] = d
    return visible


if __name__ == "__main__":
    filename = sys.argv[1]
    img = load_data(filename)

    W = 25
    H = 6
    print("Part 1:", part1(img, W, H))

    visible = part2(img, W, H)
    for i in range(0, len(visible), W):
        print(visible[i : i + W])
