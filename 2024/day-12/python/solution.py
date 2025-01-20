Point = tuple[int, int]
Blocks = list[set[Point]]


def load_input(filename: str) -> list[list[str]]:
    with open(filename) as f:
        return [list(line.strip()) for line in f.readlines()]


def build_map(topology: list[list[str]]) -> dict[str, Blocks]:
    mapping = {}
    for y, row in enumerate(topology):
        for x, c in enumerate(row):
            if c in mapping:
                added = False
                for group in mapping[c]:
                    if len(group.intersection(surrounding_points((x, y)))) > 0:
                        group.add((x, y))
                        added = True
                        break
                if not added:
                    mapping[c].append({(x, y)})
            else:
                mapping[c] = [{(x, y)}]

    for _ in range(2):  # Two passes needed to ensure all groups are merged
        for c, groups in mapping.items():
            for i in range(len(groups)):
                neighbours = []
                for g in groups[i]:
                    neighbours += surrounding_points(g)

                matches = []
                for j in range(len(groups)):
                    if j != i:
                        if len(groups[j].intersection(neighbours)) > 0:
                            matches.append(j)
                for j in matches:
                    groups[i] |= groups[j]
                    groups[j] = set()
    return mapping


def surrounding_points(p: Point) -> list[Point]:
    x, y = p
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]


def perimeter_fences(mapping: dict[str, Blocks]):
    perimeters = []
    for c, groups in mapping.items():
        for points in groups:
            neighbours = []
            for p in points:
                neighbours += list(points.intersection(surrounding_points(p)))
            perimeters.append((len(points), 4 * len(points) - len(neighbours)))
    return perimeters


def is_vertex(point: Point, points: list[Point]) -> int:
    x, y = point

    ## Thin strips and lone blocks
    surrounding_cross = {(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)}
    surrounding_block = {
        (x + 1, y),
        (x - 1, y),
        (x, y + 1),
        (x, y - 1),
        (x - 1, y - 1),
        (x - 1, y + 1),
        (x + 1, y + 1),
        (x + 1, y - 1),
    }
    cross_overlap = surrounding_cross.intersection(points)
    block_overlap = surrounding_block.intersection(points)
    horizontal_overlap = len(cross_overlap.intersection({(x - 1, y), (x + 1, y)})) == 2
    vertical_overlap = len(cross_overlap.intersection({(x, y - 1), (x, y + 1)})) == 2

    if len(cross_overlap) == 0:
        return 4

    if len(cross_overlap) == 1:
        return 2

    if len(block_overlap) == 2 and not horizontal_overlap and not vertical_overlap:
        return 2

    ## Blocks of letters
    count = 0
    # . _
    # .|x
    if (
        (x + 1, y) in points
        and (x, y + 1) in points
        and (x + 1, y + 1) not in points
    ):
        count += 1

    # x x
    # x . 
    if (
        (x - 1, y) not in points
        and (x, y - 1) not in points
        #and (x - 1, y - 1) not in points
    ):
        count += 1

    #  _ .
    #  x|.
    if (
        (x - 1, y) in points
        and (x, y + 1) in points
        and (x - 1, y + 1) not in points
    ):
        count += 1

    # x x
    # . x 
    if (
        (x + 1, y) not in points
        and (x, y - 1) not in points
        #and (x + 1, y - 1) not in points
    ):
        count += 1

    #  x|.
    #  - .
    if (
        (x - 1, y) in points
        and (x, y - 1) in points
        and (x - 1, y - 1) not in points
    ):
        count += 1

    # . x
    # x x 
    if (
        (x + 1, y) not in points
        and (x, y + 1) not in points
        #and (x + 1, y + 1) not in points
    ):
        count += 1

    #  .|x
    #  . -
    if (
        (x + 1, y) in points
        and (x, y - 1) in points
        and (x + 1, y - 1) not in points
    ):
        count += 1

    # x .
    # x x 
    if (
        (x - 1, y) not in points
        and (x, y + 1) not in points
        #and (x - 1, y + 1) not in points
    ):
        count += 1

    return count


def num_vertices(points: list[Point]) -> int:
    return sum([is_vertex(point, points) for point in points])


def bulk_discount(mapping: dict[str, Blocks]):
    perimeters = []
    for c, groups in mapping.items():
        for group in groups:
            sides = num_vertices(group)
            perimeters.append(sides * len(group))
    return perimeters


if __name__ == "__main__":
    filename = "input.txt"
    topology = load_input(filename)
    points = build_map(topology)
    perimeters = perimeter_fences(points)
    print(f"Part 1: {sum([v * p for v, p in perimeters])}")

    discount = bulk_discount(points)
    print(f"Part 2: {sum(discount)}")
    # 942474 too low
