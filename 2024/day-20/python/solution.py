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

    def get_neighbours(self, current_node: Point, visited: list[Point]) -> list[Point]:
        x, y = current_node
        candidates = {(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)}
        return self.maze.intersection(candidates) - set(visited)

    def dijkstra(self, early_exit=False) -> dict[Point, int]:
        scores = {p: 999999 for p in self.maze}
        unvisited = list(self.maze.copy())
        visited = []
        scores[self.start] = 0
        while unvisited:
            current_node = min(unvisited, key=lambda x: scores[x])
            current_score = scores[current_node]

            for neighbour in self.get_neighbours(current_node, visited):
                if current_score + 1 < scores[neighbour]:
                    scores[neighbour] = current_score + 1

            if early_exit and current_node == self.end:
                break
            unvisited.remove(current_node)
            visited.append(current_node)
        return scores[self.end]

    def cheat_race(self, threshold: int = 100) -> dict[int, int]:
        saves = 0
        baseline = self.dijkstra()
        for obstacle in tqdm(self.obstacles):
            self.maze.add(obstacle)
            score = self.dijkstra(early_exit=True)
            diff = baseline - score
            if diff >= threshold:
                saves += 1
            self.maze.remove(obstacle)
        return saves


if __name__ == "__main__":
    filename = "input.txt"
    maze = Maze(filename)
    cheats = maze.dijkstra(early_exit=False)
    print(f"Part 1: {cheats}")
