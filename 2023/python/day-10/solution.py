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
        while next_coord != S or next_coord not in coords:
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


if __name__ == "__main__":
    filename = "input.txt"
    input = load_input(filename)
    coords, S = parse_input(input)
    paths = find_path(coords, S)
    print(len(paths[0]) // 2)
