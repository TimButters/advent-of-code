from itertools import product
from functools import reduce
from operator import mul
from math import comb
import re


def load_input(filename: str):
    with open(filename) as f:
        return [
            (a, list(map(int, b.split(","))))
            for a, b in [line.split() for line in f.readlines() if line]
        ]


def num_possibilities(record):
    condition, groups = record
    cond_groups = [grp for grp in condition.split(".") if grp]
    return brute_force(condition, groups)
    #if len(cond_groups) == len(groups):
    #    return reduce(
    #        mul,
    #        [group_combinations(cond, grp) for cond, grp in zip(cond_groups, groups)],
    #    )
    # elif condition.find("#") == -1 and condition.find(".") == -1:
    #    return blank_condition(condition, groups)
    # else:
    #    return brute_force(condition, groups)


def group_combinations(cond, group):
    if len(cond) == group or cond.count("#") == group:
        return 1
    elif cond.count("#") == 0:
        return comb(len(cond), group)
    else:
        st_idx = cond.find("#")
        en_idx = cond.rfind("#")
        if st_idx == 0 or en_idx == len(cond) - 1:
            return 1
        else:
            return group - (en_idx - st_idx)


def blank_condition(condition, groups):
    min_length = sum(groups) + len(groups) - 1
    cond = len(condition)
    return sum(
        [len(groups) * (c - min_length) + 1 for c in range(cond, min_length - 1, -1)]
    )


def brute_force(condition, groups):
    N = len(condition)
    M = sum(groups) + len(groups) - 1

    dots = list(product(*[l for l in [list(range(1, N - M + 2))] * (len(groups) - 1)]))
    tests = []
    for dot in dots:
        tests.append(
            "".join(["#" * grp + "." * d for grp, d in zip(groups, list(dot) + [0])])
        )
    tests = [test for test in tests if len(test) <= N]

    combinations = 0
    for test in tests:
        for i in range(N - M + 1):
            match = True
            for s, t in zip(condition[i : i + len(test)], test):
                filled = condition[0:i] + test + condition[i + len(test) :]
                if (
                    len(filled) > N
                    or (s != t and s != "?")
                    or (len(re.findall(r"#", filled)) > sum(groups))
                ):
                    match = False
                    break
            if match:
                combinations += 1

    return combinations


if __name__ == "__main__":
    filename = "input.txt"
    input = load_input(filename)
    print(sum([num_possibilities(record) for record in input]))

    input2 = [((inp[0]+"?")*5, inp[1]*5) for inp in input]
    print(sum([num_possibilities(record) for record in input2]))
