from dataclasses import dataclass


Point = tuple[int, int]


class Guard:
    def __init__(self, start: Point, character: str):
        self.position = start
        match character:
            case "^":
                self.orientation = (0, -1)
            case ">":
                self.orientation = (1, 0)
            case "v":
                self.orientation = (0, 1)
            case "<":
                self.orienation = (-1, 0)

    def rotate(self):
        match self.orientation:
            case (0, -1):
                self.orientation = (1, 0)
            case (1, 0):
                self.orientation = (0, 1)
            case (0, 1):
                self.orientation = (-1, 0)
            case (-1, 0):
                self.orientation = (0, -1)

    def next_point(self):
        x, y = self.position
        rx, ry = self.orientation
        return (x + rx, y + ry)

    def move(self):
        x, y = self.position
        rx, ry = self.orientation
        self.position = (x + rx, y + ry)


@dataclass
class Grid:
    max_x: int
    max_y: int
    guard: Guard
    obstacles: list[Point]


def load_input(filename: str) -> Grid:
    with open(filename) as f:
        grid = f.readlines()

    max_x = len(grid[0].strip())
    max_y = len(grid)

    obstacles = []
    for y, row in enumerate(grid):
        for x, p in enumerate(row):
            if p == "#":
                obstacles.append((x, y))
            elif p in ["v", ">", "<", "^"]:
                guard = Guard((x, y), p)
    return Grid(max_x, max_y, guard, obstacles)


def track_guard(grid: Grid) -> set[Point]:
    path = [grid.guard.position]
    while True:
        next_x, next_y = grid.guard.next_point()
        if next_x < 0 or next_x >= grid.max_x or next_y < 0 or next_y >= grid.max_y:
            return set(path)
        elif (next_x, next_y) in grid.obstacles:
            grid.guard.rotate()
        else:
            grid.guard.move()
            path.append((next_x, next_y))
        if len(path) - len(set(path)) > 1000:
            return set()


if __name__ == "__main__":
    filename = "input.txt"
    grid = load_input(filename)
    path = track_guard(grid)
    print(f"Part 1: {len(path)}")
