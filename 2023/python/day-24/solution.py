from itertools import combinations
from operator import ge, le


class Hail:
    def __init__(self, x, y, z, vx, vy, vz):
        self.x0 = x
        self.y0 = y
        self.z0 = z
        self._x_comparitor = ge if vx >=0 else le
        self._y_comparitor = ge if vy >=0 else le

        t_interval = 100000
        x1 = x + (vx * t_interval)
        y1 = y + (vy * t_interval)

        self.m = (y1 - y) / (x1 - x)
        self.c = y - self.m * x

    def crossing_point(self, rhs):
        try:
            x = (self.c - rhs.c) / (rhs.m - self.m)
        except:
            return None, None
        y = self.m * x + self.c
        return x, y

    def cross_in_box(self, rhs, min, max):
        x, y = self.crossing_point(rhs)
        if x is None or y is None:
            return False
        return (
            (max >= x >= min)
            and (max >= y >= min)
            and self._x_comparitor(x, self.x0)
            and rhs._x_comparitor(x, rhs.x0)
            and self._y_comparitor(y, self.y0)
            and rhs._y_comparitor(y, rhs.y0)
        )


def load_input(filename: str):
    with open(filename) as f:
        return [
            list(map(int, line.replace("@", ",").split(","))) for line in f.readlines()
        ]


if __name__ == "__main__":
    filename = "input.txt"
    hail = [Hail(*h) for h in load_input(filename)]

    bbox_min = 7 if filename == "test_input.txt" else 200000000000000
    bbox_max = 27 if filename == "test_input.txt" else 400000000000000
    print(sum([int(a.cross_in_box(b, bbox_min, bbox_max)) for a, b in combinations(hail, r=2)]))

    # 16121 too high
