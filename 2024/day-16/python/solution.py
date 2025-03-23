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
    def _score_options(position, direction, options):
        options_scores = []
        curr_x, curr_y = position
        for option in options:
            x, y = option
            if (
                x == curr_x
                and (direction == Direction.North or direction == Direction.South)
            ) or (
                y == curr_y
                and (direction == Direction.East or direction == Direction.West)
            ):
                score = 1
            else:
                score = 1001

            options_scores.append((option, score))
        return options_scores

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

    def _neighbours(
        self, position: Point, visited: list[Point], direction: Direction | None
    ):
        x, y = position
        candidates = {(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)}
        options = candidates.intersection(self.maze) - set(visited)
        return Maze._score_options(position, direction, options)

    def dijkstra(self):
        node_scores = {p: (999999, (-1, -1), -1) for p in self.maze}
        unvisited = self.maze.copy() + [self.start]
        visited = []
        node_scores[self.start] = (0, (-1, -1), Direction.East)
        while unvisited:
            if not visited:
                current_node = self.start
            else:
                current_node = min(
                    [(node_scores[p][0], p) for p in unvisited], key=lambda x: x[0]
                )[1]

            score, _, direction = node_scores[current_node]
            for n_pos, n_score in self._neighbours(current_node, visited, direction):
                new_score = score + n_score
                if new_score < node_scores[n_pos][0]:
                    node_scores[n_pos] = (
                        new_score,
                        current_node,
                        Maze._update_direction(current_node, n_pos),
                    )
            visited.append(current_node)
            unvisited.remove(current_node)
        return node_scores


if __name__ == "__main__":
    filename = "./input.txt"
    maze = Maze(filename)
    print(maze.start, maze.end, "\n")

    paths = maze.dijkstra()
    print(f"Part 1: {paths[maze.end][0]}")
