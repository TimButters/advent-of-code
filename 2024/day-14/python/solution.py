from math import prod

Point = tuple[int, int]


class Robot:
    def __init__(self, pos: Point, vel: Point, max_x: int, max_y: int):
        self.pos = pos
        self._vel = vel
        self.max_x = max_x
        self.max_y = max_y

    def move(self, seconds: int):
        x, y = self.pos
        vx, vy = self._vel
        new_x = x + vx * seconds
        new_y = y + vy * seconds

        if new_x < 0:
            new_x = (self.max_x + new_x) % self.max_x

        if new_x >= self.max_x:
            new_x = (new_x - self.max_x) % self.max_x

        if new_y < 0:
            new_y = (self.max_y + new_y) % self.max_y

        if new_y >= self.max_y:
            new_y = (new_y - self.max_y) % self.max_y

        self.pos = (new_x, new_y)


def load_input(filename: str, max_x: int, max_y: int):
    robots = []
    with open(filename) as f:
        for line in f.readlines():
            pos, vel = [
                tuple(map(int, robot.split(",")))
                for robot in line.replace("=", "")
                .replace("p", "")
                .replace("v", "")
                .split(" ")
            ]
            assert len(pos) == 2
            assert len(vel) == 2
            robots.append(Robot(pos, vel, max_x, max_y))
    return robots


def count_quadrants(robots: list[Robot], max_x: int, max_y: int):
    counts = {
        1: 0,
        2: 0,
        3: 0,
        4: 0,
    }
    split_x = max_x // 2
    split_y = max_y // 2
    for robot in robots:
        x, y = robot.pos
        if x < split_x and y < split_y:
            counts[1] += 1
        elif x > split_x and y < split_y:
            counts[2] += 1
        elif x > split_x and y > split_y:
            counts[3] += 1
        elif x < split_x and y > split_y:
            counts[4] += 1
        else:
            pass
    return counts


def part1(robots: list[Robot], max_x: int, max_y: int) -> int:
    for robot in robots:
        robot.move(100)
    counts = count_quadrants(robots, X, Y)
    return prod(counts.values())


def part2(robots: list[Robot], X: int, Y: int):
    for i in range(10404):
        board = []
        state = set()
        for robot in robots:
            robot.move(1)
            board.append(robot.pos)
            state.add(robot.pos)
        if len(state) == len(robots):
            print(i + 1)
            print_board(board, X, Y)
            print()


def print_board(board: list[Point], X: int, Y: int):
    for y in range(Y):
        row = ""
        for x in range(X):
            row += "#" if (x, y) in board else "."
        print(row)
    print()


if __name__ == "__main__":
    filename = "input.txt"
    X = 101
    Y = 103
    robots = load_input(filename, X, Y)
    safety_factor = part1(robots, X, Y)
    print(f"Part 1: {safety_factor}")

    robots = load_input(filename, X, Y)
    part2(robots, X, Y)
