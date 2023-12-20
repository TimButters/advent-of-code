from itertools import groupby
from operator import itemgetter


def load_input(filename: str):
    with open(filename) as f:
        images = f.read().split("\n\n")
        ims = [parse_image(im) for im in images]
        return ims


def parse_image(image):
    im = []
    for y, line in enumerate(image.split("\n")):
        rows = []
        for x, c in enumerate(list(line)):
            rows.append(c)
        if rows:
            im.append(rows)
    return im


def find_reflection(image, skip=None):
    if skip is not None and skip >= 100:
        idx = _find_reflection(image, skip // 100)
    else:
        idx = _find_reflection(image)

    if idx is None:
        if skip is not None and skip < 100:
            idx = _find_reflection(pivot(image), skip)
        else:
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
        for i in range(len(image)):
            if image[i] == ".":
                new_char = "#"
            elif image[i] == "#":
                new_char = "."
            else:
                continue
            new_image = image[0:i] + new_char + image[i+1:]
            idx = find_reflection(parse_image(new_image), skip=orig_idx)
            if idx is not None and idx != orig_idx:
                idxs.append(idx)
                break
    return idxs


def _find_reflection(image, skip=-1):
    M = len(image)
    for i in range(1, M):
        if i == skip:
            continue
        if image[i] == image[i-1]:
            if check_reflection(image[i-1::-1], image[i:]):
                return i
    return None


def check_reflection(im1, im2):
    for r1, r2 in zip(im1, im2):
        if not r1 == r2:
            return False
    return True


def pivot(image):
    return [list(im) for im in list(zip(*image))]


if __name__ == "__main__":
    filename = "input.txt"
    images = load_input(filename)
    print(sum([find_reflection(image) for image in images]))

    print(sum(part2(filename)))
