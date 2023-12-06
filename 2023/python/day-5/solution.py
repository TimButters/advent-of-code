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


def find_intersection(k1, k2):
    k1_st, k1_en = k1
    k2_st, k2_en = k2
    return (k1_st >= k2_st and k1_st <= k2_en) or (k2_en >= k1_st and k2_en <= k1_en)


def find_ranges(mappings):
    paths = list(mappings.keys())[1:]
    location_ranges = [(1459646077, 1579893769)]
    # location_ranges = [
    #     (1459646077, 1579893769),
    #     (2545941209, 2580528551),
    #     (224099516, 375808933),
    #     (181690176, 224099516),
    #     (2580528551, 2731943364),
    #     (393220323, 444572577),
    #     (2731943364, 2812721094),
    #     (2492137689, 2545941209),
    #     (2334599269, 2427978815),
    #     (1579893769, 1729601904),
    #     (960858811, 1166524785)
    # ]
    location_ranges = [(0, 56)]
    for path in paths[-2::-1]:
        new_location_ranges = []
        for loc in location_ranges:
            new_location_ranges.append([(kst, ken) for (kst, ken), v in mappings[path].items() if find_intersection((kst+v, ken+v), loc)])

        location_ranges = {n for r in new_location_ranges for n in r}
    return list(location_ranges)


def select_seeds(seed_ranges, ranges):
    #seeds = [s for st, en in zip(seed_ranges[::2], seed_ranges[1::2]) for s in list(range(st, st+en))]
    seeds = [s for rst, ren in ranges for st, en in zip(seed_ranges[::2], seed_ranges[1::2]) for s in list(range(st, st+en)) if (st >= rst and st <=ren) or (en >= rst and en <= ren)]
    # Find the seed production set that correspond to the ranges then just generate those??
    return seeds  #[seed for rst, ren in ranges for seed in seeds if seed >= rst and seed <= ren]


def part2(mappings):
    ranges = find_ranges(mappings)
    seeds = select_seeds(mappings["seeds"], ranges)
    paths = list(mappings.keys())[1:]
    min_path = 10**24
    for seed in seeds:
        val = seed
        for path in paths:
            val = map_value(val, mappings[path])
        if val < min_path:
            min_path = val
    return min_path


if __name__ == "__main__":
    filename = "test_input.txt"
    maps = load_input(filename)
    print(part1(maps))
    print(part2(maps))

    # 599623118 too high
    # 457535844 too high
