from enum import Enum

Point = tuple[int, int]


class Direction(Enum):
    North = 1
    South = 2
    East = 3
    West = 4


class Maze:
    def __init__(self, filename: str):
        self.start = None
        self.end = None
        self.maze = []
        self._load_maze(filename)

    def _load_maze(self, filename: str):
        with open(filename) as f:
            lines = f.readlines()

        for y, row in enumerate(lines[::-1]):
            for x, c in enumerate(row):
                if c == ".":
                    self.maze.append((x, y))
                if c == "S":
                    self.start = (x, y)
                if c == "E":
                    self.maze.append((x, y))
                    self.end = (x, y)

    @staticmethod
    def score_path(path: list[tuple[Point, int]]):
        return sum([p[1] for p in path])

    @staticmethod
    def _score_options(position, direction, options):
        options_scores = []
        curr_x, curr_y = position
        for option in options:
            x, y = option
            if (
                x == curr_x
                and (direction == Direction.North)
                or direction == Direction.South
            ) or (
                y == curr_y
                and (direction == Direction.East or direction == Direction.West)
            ):
                score = 1
            else:
                score = 1001

            options_scores.append((option, score))
        return options_scores

    def _find_next_cells(
        self, position: Point, direction: Direction, path: list[Point]
    ):
        x, y = position
        candidates = {(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)}
        options = candidates.intersection(self.maze) - set([p[0] for p in path])
        return self._score_options(position, direction, options)

    @staticmethod
    def _update_direction(position: Point, next_step: Point):
        x, y = position
        new_x, new_y = next_step
        if new_x > x:
            return Direction.East
        elif new_x < x:
            return Direction.West
        elif new_y > y:
            return Direction.North
        else:
            return Direction.South

    def walk(self):
        position = self.start
        direction = Direction.East
        path = [(self.start, 0)]
        paths = []
        decisions = []
        decision_point = False
        step = 0
        while True:
            if not decision_point:
                options = self._find_next_cells(position, direction, path)
                next_step = min(options, key=lambda x: x[1]) if options else None

                decision_set = list(set(options) - {next_step})
                if decision_set:
                    decisions.append((position, direction, step, decision_set))
            decision_point = False

            if next_step is None:
                position, direction, step, decision_set = decisions[-1]
                next_step = decision_set.pop()
                if not decision_set:
                    decisions.pop()
                decision_point = True
                paths.append(path)
                path = path[0 : step + 1]
            elif next_step[0] == self.end:
                path.append(next_step)
                return path
            else:
                direction = self._update_direction(position, next_step[0])
                path.append(next_step)
                position = next_step[0]
                step += 1


if __name__ == "__main__":
    filename = "./test_input.txt"
    maze = Maze(filename)
    print(maze.start, maze.end, "\n")
    path = maze.walk()
    print(path)
    print(len(path))
    print(Maze.score_path(path))
