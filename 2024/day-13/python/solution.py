from math import isclose
import re


Point = tuple[int, int]


def load_input(filename: str) -> list[tuple[Point, Point, Point]]:
    machines = []
    with open(filename) as f:
        machines_string = f.read().strip().split("\n\n")
        for machine in machines_string:
            nums = [int(n) for n in re.findall(r"\d+", machine)]
            a = tuple(nums[0:2])
            b = tuple(nums[2:4])
            target = tuple(nums[4:6])
            machines.append((a, b, target))
    return machines


def minimum_presses(a: Point, b: Point, target: Point) -> tuple[float, float]:
    """
    a*ax + b*bx = x
    a*ay + b*by = y
    b = y - a*ay / by
    a*ax + bx*y/by - a*ay*bx/by = x
    x - bx*y/by = a*ax - a*ay*bx/by
    x - bx*y/by = a*(ax - ay*bx/by)
    x - bx*y/by / (ax - ay*bx/by) = a
    """
    ax, ay = a
    bx, by = b
    target_x, target_y = target
    a_coeff = (target_x - (bx * target_y) / by) / (ax - (ay * bx) / by)
    b_coeff = (target_y - a_coeff * ay) / by
    return a_coeff, b_coeff


def cost_to_prize(a: Point, b: Point, target: Point, factor=None) -> int:
    if factor is None:
        a_press, b_press = minimum_presses(a, b, target)
    else:
        tx, ty = target
        a_press, b_press = minimum_presses(a, b, (tx + factor, ty + factor))

    if (
        isclose(round(a_press), a_press, rel_tol=0, abs_tol=1e-4)
        and isclose(round(b_press), b_press, rel_tol=0, abs_tol=1e-4)
        and a_press > 0
        and b_press > 0
    ):
        return 3 * round(a_press) + round(b_press)
    else:
        return 0


if __name__ == "__main__":
    filename = "input.txt"
    machines = load_input(filename)
    cost = [cost_to_prize(*machine) for machine in machines]
    print(f"Part 1: {sum(cost)}")

    cost = [cost_to_prize(*machine, factor=10000000000000) for machine in machines]
    print(f"Part 2: {sum(cost)}")

