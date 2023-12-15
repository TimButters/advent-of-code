def load_input(filename: str):
    with open(filename) as f:
        return [list(line.strip()) for line in f.readlines() if line]


def parse_input(input):
    S = None
    coords = {}
    for y, line in enumerate(input):
        for x, c in enumerate(line):
            if c == ".":
                continue
            elif c == "-":
                if x == 0:
                    continue
                coords[(x, y)] = [(x - 1, y), (x + 1, y)]
            elif c == "|":
                if y == 0:
                    continue
                coords[(x, y)] = [(x, y - 1), (x, y + 1)]
            elif c == "7":
                if x == 0:
                    continue
                coords[(x, y)] = [(x - 1, y), (x, y + 1)]
            elif c == "F":
                coords[(x, y)] = [(x + 1, y), (x, y + 1)]
            elif c == "L":
                if y == 0:
                    continue
                coords[(x, y)] = [(x, y - 1), (x + 1, y)]
            elif c == "J":
                if y == 0:
                    continue
                coords[(x, y)] = [(x, y - 1), (x - 1, y)]
            elif c == "S":
                S = (x, y)
            else:
                raise ValueError("Unrecognised symbol", c)
    return coords, S


def find_path(coords, S):
    paths = []
    starts = [k for k, v in coords.items() if S in v]
    for start in starts:
        current_coord = start
        next_coord = [coord for coord in coords[start] if coord not in [S, start]][0]
        path = [S, start, next_coord]
        while next_coord != S:
            current_coord = next_coord
            next_coord_options = [
                coord for coord in coords[current_coord] if coord not in path
            ]
            if not next_coord_options:
                break
            next_coord = next_coord_options[0]
            path.append(next_coord)
        paths.append(path)
    return paths


def enclosed_area(path):
    bounding_box_xmin = min(path, key=lambda c: c[0])[0]
    bounding_box_xmax = max(path, key=lambda c: c[0])[0]
    bounding_box_ymin = min(path, key=lambda c: c[1])[1]
    bounding_box_ymax = max(path, key=lambda c: c[1])[1]

    area = []
    for x in range(bounding_box_xmin, bounding_box_xmax):
        for y in range(bounding_box_ymin, bounding_box_ymax):
            if (x, y) in path:
                continue
            edge_count = sum([1 for xtest, ytest in path if xtest > x and ytest == y])
            if edge_count % 2:
                print(edge_count)
                area.append((x, y))
    return area



if __name__ == "__main__":
    filename = "test_input.txt"
    input = load_input(filename)
    coords, S = parse_input(input)
    paths = find_path(coords, S)
    print(len(paths[0]) // 2)
