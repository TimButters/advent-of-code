from collections import deque
from dataclasses import dataclass


@dataclass
class Space:
    start: int
    end: int
    size: int


@dataclass
class File:
    start: int
    end: int
    size: int
    value: int


def load_input(filename: str):
    with open(filename) as f:
        line = list(map(int, f.read().strip()))

    space = deque()
    spaces = []
    files = []

    disk = [-1] * 100000
    filetype = True
    offset = 0
    value = 0
    for size in line:
        if filetype:
            files.append(
                    File(start=offset, end=offset+size, size=size, value=value)
            )
            disk[offset: offset + size] = [value] * size
            value += 1
        else:
            _ = [space.append(offset + i) for i in range(size)]
            spaces.append(Space(start=offset, end=offset+size, size=size))
        filetype = not filetype
        offset += size
    disk = disk[0: offset]

    return disk, space, files, spaces


def defrag(disk, space):
    idx = len(disk) - 1
    while space:
        while disk[idx] == -1:
            idx -= 1

        space_idx = space.popleft()
        disk[space_idx] = disk[idx]
        disk[idx] = -2
        idx -= 1
    defrag_idx = disk.index(-2)
    extra = [d for d in disk[defrag_idx:] if d >= 0]
    return disk[0: idx + 1] + extra[::-1]


def defrag_files(disk, files, spaces):
    for file in files[::-1]:
        for space in spaces:
            if space.size >= file.size and space.start < file.start:
                disk[space.start: space.start + file.size] = disk[file.start: file.end]
                disk[file.start: file.end] = [-2] * file.size
                space.size -= file.size
                space.start += file.size
                break
    return disk


def checksum(disk):
    return sum([i * v for i, v in enumerate(disk) if v >= 0])


if __name__ == "__main__":
    filename = "input.txt"
    disk, space, files, spaces = load_input(filename)
    df = defrag(disk.copy(), space)
    print(f"Part 1: {checksum(df)}")

    d2 = defrag_files(disk, files, spaces)
    print(f"Part 2: {checksum(d2)}")

