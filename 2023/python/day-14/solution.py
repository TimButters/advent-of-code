from operator import itemgetter
from numpy import diff


def load_input(filename: str):
    with open(filename) as f:
        lines = [list(line.strip()) for line in f.readlines() if line]
        N = len(lines[0])
        return [
            [(x, y, c) for x, c in enumerate(line) if c != "."]
            for y, line in enumerate(lines)
        ], N


def tilt_platform(input):
    new_input = [inp for inp in input[0]]
    for line in input[1:]:
        for x, y, r in line:
            if r == "#":
                new_input.append((x, y, r))
                continue
            y_new = y
            while y_new > 0 and (x, y_new-1, "O") not in new_input and (x, y_new-1, "#") not in new_input:
                y_new -= 1
            new_input.append((x, y_new, r))
    return new_input


def tilt_platform_south(input):
    M = len(input)
    new_input = [inp for inp in input[-1]]
    for line in input[::-1][1:]:
        for x, y, r in line:
            if r == "#":
                new_input.append((x, y, r))
                continue
            y_new = y
            while y_new < (M-1) and (x, y_new+1, "O") not in new_input and (x, y_new+1, "#") not in new_input:
                y_new += 1
            new_input.append((x, y_new, r))
    return new_input


def tilt_platform_east(input, N):
    new_input = []
    for line in input:
        for x, y, r in sorted(line, reverse=True):
            if r == "#":
                new_input.append((x, y, r))
                continue
            x_new = x
            while x_new < (N-1) and (x_new+1, y, "O") not in new_input and (x_new+1, y, "#") not in new_input:
                x_new +=1
            new_input.append((x_new, y, r))
    return new_input


def tilt_platform_west(input):
    new_input = []
    for line in input:
        for x, y, r in sorted(line):
            if r == "#":
                new_input.append((x, y, r))
                continue
            x_new = x
            while x_new > 0 and (x_new-1, y, "O") not in new_input and (x_new-1, y, "#") not in new_input:
                x_new -=1
            new_input.append((x_new, y, r))
    return new_input


def build_image(coords):
    image = []
    row = []
    current_row = 0
    for x, y, r in sorted(coords, key=itemgetter(1)):
        if y > current_row:
            current_row += 1
            image.append(sorted(row))
            row = []
        row.append((x, y, r))
    image.append(sorted(row))
    return image


def tilt_round(image, N, iterations=1):
    loads = []
    for i in range(iterations):
        image = tilt_platform(image)
        image = build_image(image)
        image = tilt_platform_west(image)
        image = build_image(image)
        image = tilt_platform_south(image)
        image = build_image(image)
        image = tilt_platform_east(image, N)
        image = build_image(image)
        loads.append(sum([len(input) - y for _, y, r in [im for line in image for im in line] if r == "O"]))
    return loads


if __name__ == "__main__":
    filename = "input.txt"
    input, N = load_input(filename)
    new_input = tilt_platform(input)
    print(sum([len(input) - y for _, y, r in new_input if r == "O"]))

    loads = tilt_round(input, N, iterations=150)
    for i in range(150):
        diffs = diff([idx for idx, v in enumerate(loads) if v == loads[i]])
        if len(diff(diffs)) and (not any(diff(diffs))):
            cycle_start = i
            cycle_length = diffs[0]
            break
    offset = (1000000000 - cycle_start) % cycle_length
    print(loads[cycle_start + offset - 1])
