from math import lcm
from itertools import cycle


def load_input(filename: str):
    with open(filename) as f:
        return [line.strip() for line in f.readlines() if line.strip()]


def process_links(lines: list[str]):
    # return {key: {"L": v1[1:], "R": v2[:-1]} for line in lines for key, _, v1, v2 in line.split()}
    d = {}
    for line in lines:
        key, _, left, right = line.split()
        d[key] = {"L": left[1:-1], "R": right[:-1]}
    return d


def find_zzz(links, directions):
    current_node = "AAA"
    for counter, direction in enumerate(cycle(directions), start=1):
        current_node = links[current_node][direction]
        if current_node == "ZZZ":
            return counter


def find_steps_to_z(links, directions):
    current_nodes = [k for k in links.keys() if k[-1] == "A"]
    counters = []
    for current_node in current_nodes:
        for counter, direction in enumerate(cycle(directions), start=1):
            current_node = links[current_node][direction]
            if current_node[-1] == "Z":
                counters.append(counter)
                break
    return counters



if __name__ == "__main__":
    filename = "input.txt"
    input = load_input(filename)
    directions = input[0]
    links = process_links(input[1:])
    count = find_zzz(links, directions)
    print(count)

    counters = find_steps_to_z(links, directions)
    print(lcm(*counters))
