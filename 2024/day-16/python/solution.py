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
                    self.end = (x, y)

    @staticmethod
    def _score_options(position, direction, options):
        options_scores = []
        curr_x, curr_y = position
        for option in options:
            x, y = option
            if (
                x == curr_x
                and (direction == Direction.East)
                or direction == Direction.West
            ) or (
                y == curr_y
                and (direction == Direction.North or direction == Direction.South)
            ):
                score = 1
            else:
                score = 1000

            options_scores.append((option, score))
        return options_scores

    def _find_next_cells(self, position: Point, direction: Direction):
        x, y = position
        candidates = {(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)}
        options = candidates.intersection(self.maze) ^ self.path
        return self._score_options(options, direction)

    def walk(self):
        path = []
        position = self.start
        direction = Direction.East
        options = self._find_next_cells(position, direction)


if __name__ == "__main__":
    filename = "./test_input.txt"
    maze = Maze(filename)
    print(maze.start, maze.end)
    print(maze.maze)
