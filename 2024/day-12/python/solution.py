from itertools import groupby


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


# def num_sides(points: list[Point], axis, output):
#     """NO: Doesn't work as doesn't account for blocks of the same letter"""
#     ps = sorted(points, key=lambda x: x[axis])
#     diffs = [b[axis] - a[axis] for a, b in zip(ps, ps[1:])]
#     onside = False
#     num_side = 0
#     for diff in diffs:
#         if diff == 1:
#             if not onside:
#                 num_side += 1
#                 onside = True
#         else:
#             onside = False
#     if output:
#         print(axis, num_side, list(ps))
#     return num_side


def is_vertex(point: Point, points: list[Point]) -> bool:
    x, y = point

    # . _
    # .|x
    if (x+1, y) in points and (x, y+1) in points and ((x-1, y-1) not in points or (x+1, y+1) not in points):
        return True

    #  _ .
    #  x|.
    if (x-1, y) in points and (x, y+1) in points and ((x+1, y-1) not in points or (x-1, y+1) not in points):
        return True

    #  x|.
    #  - .
    if (x-1, y) in points and (x, y-1) in points and ((x+1, y+1) not in points or (x-1, y-1) not in points):
        return True

    #  .|x
    #  . -
    if (x+1, y) in points and (x, y-1) in points and ((x+1, y-1) not in points or (x-1, y+1) not in points):
        return True

    return False


def num_vertices(points: list[Point]) -> int:
    return sum([is_vertex(point, points) for point in points])


def bulk_discount(mapping: dict[str, Blocks]):
    perimeters = []
    for c, groups in mapping.items():
        for group in groups:
            sides = num_vertices(group)
            print(f"{c}: \t{len(group)}\t{sides}")
            perimeters.append(sides * len(group))
    return perimeters


if __name__ == "__main__":
    filename = "test_input3.txt"
    topology = load_input(filename)
    points = build_map(topology)
    perimeters = perimeter_fences(points)
    print(f"Part 1: {sum([v * p for v, p in perimeters])}")

    discount = bulk_discount(points)
    print(f"Part 2: {discount}")
