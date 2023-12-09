from math import sqrt


def build_map(line: str):
    left, right = line.split(":")
    map_name = left.split()[0]
    if map_name == "seeds":
        return [(map_name, list(map(int, right.strip().split())))]
    return [
        (
            map_name,
            {
                (int(source), int(source) + int(length)): int(dest) - int(source)
                for dest, source, length in map(str.split, right.strip().split("\n"))
            },
        )
    ]


def build_map2(line: str):
    left, right = line.split(":")
    map_name = left.split()[0]
    if map_name == "seeds":
        return [(map_name, list(map(int, right.strip().split())))]
    return [
        (
            map_name,
            {
                (int(source), int(source) + int(length)): int(dest) - int(source)
                for source, dest, length in map(str.split, right.strip().split("\n"))
            },
        )
    ]


def load_input(filename: str, builder=build_map):
    with open(filename) as f:
        return {s: d for m in f.read().split("\n\n") for (s, d) in builder(m)}


def map_value(value: int, mapping) -> int:
    match = [(ks, kd) for (ks, kd) in mapping.keys() if ks <= value < kd]
    return value if not match else value + mapping[match[0]]


def part1(mappings, seeds=None):
    if seeds is None:
        seeds = mappings["seeds"]
    paths = list(mappings.keys())[1:]
    min_path = 10**24
    for seed in seeds:
        val = seed
        for path in paths:
            val = map_value(val, mappings[path])
        if val < min_path:
            min_path = val
    return min_path


def part2(mappings, loc_range):
    paths = list(mappings.keys())[-1:0:-1]
    seed_set = []
    for loc in loc_range:
        val = loc
        for path in paths:
            val = map_value(val, mappings[path])
        seed_set.append(val)
    return seed_set


def get_ranges(seeds, seed_ranges):
    ranges = []
    for start, length in zip(seed_ranges[::2], seed_ranges[1::2]):
        for seed in seeds:
            if start <= seed < (start + length):
                ranges.append((start, length, seed))
    return ranges


def get_seeds(seed_ranges):
    return [s for st, en in zip(seed_ranges[::2], seed_ranges[1::2]) for s in list(range(st, st+en))]


if __name__ == "__main__":
    filename = "input.txt"
    maps = load_input(filename)
    print(part1(maps))

    maps2 = load_input(filename, builder=build_map2)
    st = 0
    en = 120247692
    seed_set = part2(maps2, range(st, en, (en - st) // 500000))
    ranges = get_ranges(seed_set, maps2["seeds"])
    min_r = min([r[2] for r in ranges])
    max_r = max([r[2] for r in ranges])
    print(part1(maps, seeds=list(range(min_r-500, min_r))))

    # 41222968
