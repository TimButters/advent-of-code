from dataclasses import dataclass


Point = tuple[int, int]


@dataclass
class Plan:
    walls: set[Point]
    obstacles: set[Point]
    robot: Point


def load_input(filename: str):
    with open(filename) as f:
        input = f.read()
    plan_schematic, movements = input.split("\n\n")
    plan = process_plan(plan_schematic)
    movements = movements.replace("\n", "")
    return plan, movements


def process_plan(plan_schematic):
    plan = Plan(set(), set(), (-1, -1))
    for y, row in enumerate(plan_schematic.split("\n")):
        for x, c in enumerate(row):
            if c == "#":
                plan.walls.add((x, y))

            if c == "O":
                plan.obstacles.add((x, y))

            if c == "@":
                plan.robot = (x, y)
    return plan


def process_movement(position, movement):
    x, y = position
    match movement:
        case "^":
            return (x, y - 1)
        case "v":
            return (x, y + 1)
        case "<":
            return (x - 1, y)
        case ">":
            return (x + 1, y)
        case _:
            raise ValueError(f"Unknown movement: {movement}")


def run(plan: Plan, movements: str):
    for movement in movements:
        position = plan.robot
        coord = process_movement(position, movement)
        in_wall = coord in plan.walls
        in_obstacle = coord in plan.obstacles
        if not in_wall and not in_obstacle:
            plan.robot = coord

        if not in_wall and in_obstacle:
            # obstacle_coord = process_movement(coord, movement)
            # obstacles_to_move = [coord, obstacle_coord]
            # if obstacle_coord in plan.walls:
            #     obstacles_to_move = []
            obstacle_coord = coord
            obstacles_to_move = [coord]
            while obstacle_coord in plan.obstacles:
                obstacle_coord = process_movement(obstacle_coord, movement)
                obstacles_to_move.append(obstacle_coord)
                if obstacle_coord in plan.walls:
                    obstacles_to_move = []

            if obstacles_to_move:
                plan.obstacles.remove(obstacles_to_move[0])
                plan.obstacles.add(obstacles_to_move[-1])
                plan.robot = coord


def box_gps(boxes: set[Point]) -> int:
    return sum([100 * y + x for x, y in boxes])


if __name__ == "__main__":
    filename = "input.txt"
    plan, movements = load_input(filename)
    run(plan, movements)
    print(f"Part 1: {box_gps(plan.obstacles)}")
