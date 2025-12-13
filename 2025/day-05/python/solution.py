import sys


def load_input(filename: str) -> tuple[list[tuple[int, ...]], list[int]]:
    with open(filename) as f:
        content = f.read().strip()
    ranges, ingredients = content.split("\n\n")
    ranges = sorted([tuple(map(int, r.split("-"))) for r in ranges.split()])
    ingredients = [int(i) for i in ingredients.split("\n")]
    return ranges, ingredients


def isfresh(ingredient: int, ranges: list[tuple[int, ...]]) -> bool:
    min_r = ranges[0][0]
    max_r = ranges[-1][1]

    if ingredient < min_r or ingredient > max_r:
        return False

    for s, e in ranges:
        if ingredient < s:
            return False

        if ingredient >= s and ingredient <= e:
            return True
    return False


def part1(ingredients: list[int], ranges: list[tuple[int, ...]]) -> int:
    return sum([isfresh(ingredient, ranges) for ingredient in ingredients])


def merge_ranges(ranges: list[tuple[int, ...]]) -> list[tuple[int, ...]]:
    combined = []
    s, e = ranges[0]
    for st, en in ranges[1:]:
        if e < st:
            combined.append((s, e))
            s = st
            e = en
        elif e >= st and e <= en:
            e = en
        elif e > en:
            pass
        else:
            print(s, e, st, en)
    combined.append((s, e))
    return combined


def part2(ranges: list[tuple[int, ...]]) -> int:
    return sum([e - s + 1 for s, e in ranges])

        
if __name__ == "__main__":
    filename = sys.argv[1]
    ranges, ingredients = load_input(filename)

    print("Part 1:", part1(ingredients, ranges))

    new_ranges = merge_ranges(ranges)
    print("Part 2:", part2(new_ranges))

