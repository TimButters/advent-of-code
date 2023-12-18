from itertools import groupby
from operator import itemgetter


def load_input(filename: str):
    with open(filename) as f:
        images = f.read().split("\n\n")
        ims = []
        for image in images:
            im = []
            for y, line in enumerate(image.split("\n")):
                rows = []
                for x, c in enumerate(list(line)):
                    if c == "#":
                        rows.append((x, y))
                if rows:
                    im.append(rows)
            ims.append(im)
        return ims


def parse_image(image):
    im = []
    for y, line in enumerate(image.split("\n")):
        rows = []
        for x, c in enumerate(list(line)):
            if c == "#":
                rows.append((x, y))
        if rows:
            im.append(rows)
    return im


def find_reflection(image):
    idx = _find_reflection(image)
    if idx is None:
        idx = _find_reflection(pivot(image))
    else:
        idx = 100 * idx
    return idx


def part2(filename):
    with open(filename) as f:
        images = f.read().split("\n\n")

    idxs = []
    for image in images:
        orig_idx = find_reflection(parse_image(image))
        print(len(image))
        for i in range(len(image)):
            if image[i] == ".":
                new_char = "#"
            elif image[i] == "#":
                new_char = "."
            else:
                continue
            new_image = image[0:i] + new_char + image[i+1:]
            idx = find_reflection(parse_image(new_image))
            if idx is not None and idx != orig_idx:
                print(i, "done")
                idxs.append(idx)
                break
    return idxs


def _find_reflection(image):
    M = len(image)
    for i in range(1, M):
        if [x for x, _ in image[i]] == [x for x, _ in image[i-1]]:
            if check_reflection([[x for x, _ in row] for row in image[i-1::-1]], [[x for x, _ in row] for row in image[i:]]):
                return i
    return None


def check_reflection(im1, im2):
    for r1, r2 in zip(im1, im2):
        if not r1 == r2:
            return False
    return True


def pivot(image):
    coords = [(y, x) for line in image for x, y in line]
    coords.sort(key = itemgetter(1))
    return [list(grp) for _, grp in groupby(coords, key=itemgetter(1))]




if __name__ == "__main__":
    filename = "input.txt"
    images = load_input(filename)
    print(sum([find_reflection(image) for image in images]))

    print(sum(part2(filename)))

    #30449
