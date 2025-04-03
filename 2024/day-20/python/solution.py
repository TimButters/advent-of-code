from itertools import product
from tqdm import tqdm

Point = tuple[int, int]


class Maze:
    def __init__(self, filename: str):
        self.start = None
        self.end = None
        self.obstacles = []
        self.maze = set(self._load_maze(filename))

    def _load_maze(self, filename: str) -> list[Point]:
        with open(filename) as f:
            lines = f.readlines()

        max_x = len(lines[0]) - 1
        max_y = len(lines) - 1

        maze = []
        for y, row in enumerate(lines):
            for x, c in enumerate(row):
                if c == ".":
                    maze.append((x, y))
                elif c == "S":
                    maze.append((x, y))
                    self.start = (x, y)
                elif c == "E":
                    maze.append((x, y))
                    self.end = (x, y)
                else:
                    if x > 0 and x < max_x and y > 0 and y < max_y:
                        self.obstacles.append((x, y))
        return maze

    def get_neighbours(self, current_node: Point, visited: list[Point]) -> set[Point]:
        x, y = current_node
        candidates = {(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)}
        return self.maze.intersection(candidates) - set(visited)

    def dijkstra(
        self, *, backwards: bool = False, early_exit: bool = False
    ) -> dict[Point, int]:
        assert self.start is not None
        assert self.end is not None
        if backwards:
            end = self.start
            start = self.end
        else:
            start = self.start
            end = self.end
        scores = {p: 999999 for p in self.maze}
        unvisited = list(self.maze.copy())
        visited = []
        scores[start] = 0
        while unvisited:
            current_node = min(unvisited, key=lambda x: scores[x])
            current_score = scores[current_node]

            for neighbour in self.get_neighbours(current_node, visited):
                if current_score + 1 < scores[neighbour]:
                    scores[neighbour] = current_score + 1

            if early_exit and current_node == end:
                break
            unvisited.remove(current_node)
            visited.append(current_node)
        return scores

    def cheat_race(self, threshold: int = 100) -> int:
        assert self.end is not None
        saves = 0
        baseline = self.dijkstra()[self.end]
        for obstacle in tqdm(self.obstacles):
            self.maze.add(obstacle)
            score = self.dijkstra(early_exit=True)[self.end]
            diff = baseline - score
            if diff >= threshold:
                saves += 1
            self.maze.remove(obstacle)
        return saves

    def cheat_candidates(self, obstacle: Point) -> set[Point]:
        x, y = obstacle
        surrounding = {(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)}
        return surrounding.intersection(self.maze)


if __name__ == "__main__":
    filename = "input.txt"
    maze = Maze(filename)
    scores_f = maze.dijkstra()
    scores_b = maze.dijkstra(backwards=True)

    assert maze.start is not None
    assert maze.end is not None
    cheats = dict()
    baseline = scores_f[maze.end]
    for obstacle in maze.obstacles:
        candidates = maze.cheat_candidates(obstacle)
        if len(candidates) > 1:
            for a, b in product(candidates, candidates):
                if a == b:
                    continue
                new_score = scores_f[a] + scores_b[b] + 2
                if new_score < baseline:
                    if obstacle not in cheats:
                        cheats[obstacle] = new_score
                    else:
                        cheats[obstacle] = min(new_score, cheats[obstacle])

    saves = dict()
    for _, v in cheats.items():
        diff = baseline - v
        if diff in saves:
            saves[diff] += 1
        else:
            saves[diff] = 1

    c = [
        (obstacle, score)
        for obstacle, score in cheats.items()
        if score <= baseline - 100
    ]
    print(f"Part 1: {len(c)}")
