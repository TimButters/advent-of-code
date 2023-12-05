def load_input(filename: str):
    with open(filename) as f:
        return {s: d for m in f.read().split("\n\n") for (s, d) in build_map(m)}


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


def map_value(value: int, mapping) -> int:
    match = [(ks, kd) for (ks, kd) in mapping.keys() if ks <= value < kd]
    return value if not match else value + mapping[match[0]]


def part1(mappings):
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


def part2slow(mappings):
    seed_ranges = mappings["seeds"]
    seeds = [s for st, en in zip(seed_ranges[::2], seed_ranges[1::2]) for s in list(range(st, st+en))]
    paths = list(mappings.keys())[1:]
    min_path = 10**24
    for seed in seeds:
        val = seed
        for path in paths:
            val = map_value(val, mappings[path])
        if val < min_path:
            min_path = val
    return min_path


def find_bandings(mappings):
    paths = list(mappings.keys())[1:]
    for path in paths:




if __name__ == "__main__":
    filename = "input.txt"
    maps = load_input(filename)
    print(part1(maps))
    print(part2(maps))
