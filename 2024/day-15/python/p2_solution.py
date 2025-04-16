from dataclasses import dataclass


Point = tuple[int, int]


@dataclass
class Plan:
    walls: set[Point]
    obstacles: set[tuple[Point, Point]]
    robot: Point


def print_plan(plan: Plan):
    X = max(plan.walls, key=lambda r: r[0])[0]
    Y = max(plan.walls, key=lambda r: r[1])[1]

    for y in range(Y + 1):
        row = ""
        for x in range(X + 1):
            if (x, y) in plan.walls:
                c = "#"
            elif (x, y) == plan.robot:
                c = "@"
            elif (x, y) in [b[0] for b in plan.obstacles]:
                c = "["
            elif (x, y) in [b[1] for b in plan.obstacles]:
                c = "]"
            else:
                c = "."
            row += c
        print(row)



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
        x = 0
        for c in row:
            if c == "#":
                plan.walls.add((x, y))
                plan.walls.add((x + 1, y))
                x += 2
            elif c == "O":
                plan.obstacles.add(((x, y), (x + 1, y)))
                x += 2
            elif c == "@":
                plan.robot = (x, y)
                x += 2
            else:
                x += 2
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


def process_box_movement(obstacle: tuple[Point, Point], movement):
    st, en = obstacle
    st_x, st_y = st
    en_x, en_y = en
    match movement:
        case "^":
            return ((st_x, st_y - 1), (en_x, en_y - 1))
        case "v":
            return ((st_x, st_y + 1), (en_x, en_y + 1))
        case "<":
            return ((st_x - 1, st_y), (en_x - 1, en_y))
        case ">":
            return ((st_x + 1, st_y), (en_x + 1, en_y))
        case _:
            raise ValueError(f"Unknown movement: {movement}")


def find_touching_boxes(
    obstacle: tuple[Point, Point], movement: str, obstacles: set[tuple[Point, Point]]
):
    st, en = process_box_movement(obstacle, movement)
    boxes = []
    for obs in obstacles:
        if obs == obstacle:
            continue
        x, y = obs
        if x == st or x == en or y == st or y == en:
            boxes.append(obs)
    return boxes


def is_in_obstacle(coord: Point, obstacles: set[tuple[Point, Point]]):
    x, y = coord
    if coord in [ob[0] for ob in obstacles]:
        return ((x, y), (x + 1, y))
    elif coord in [ob[1] for ob in obstacles]:
        return ((x - 1, y), (x, y))
    else:
        return None


def is_box_touching_wall(box: tuple[Point, Point], movement: str, walls: set[Point]):
    st, en = process_box_movement(box, movement)
    return st in walls or en in walls


# def run(plan: Plan, movements: str):
#    for movement in movements:
#        position = plan.robot
#        coord = process_movement(position, movement)
#        in_wall = coord in plan.walls
#        obstacle = is_in_obstacle(coord, plan.obstacles)
#        in_obstacle = obstacle is not None
#        if not in_wall and not in_obstacle:
#            plan.robot = coord
#
#        if not in_wall and in_obstacle:
#            stuck = False
#            assert obstacle is not None
#            # obstacle_coord = coord
#            obstacles_to_move = [obstacle]
#            obstacles = [obstacle]
#            while obstacles:
#                new_obstacles = []
#                for obstacle in obstacles:
#                    if is_box_touching_wall(obstacle, movement, plan.walls):
#                        obstacles_to_move = []
#                        stuck = True
#                        break
#
#
#                    new_obstacles += find_touching_boxes(obstacle, movement, plan.obstacles)
#                    obstacles_to_move += new_obstacles
#
#                if stuck:
#                    break
#
#                obstacles = new_obstacles
#
#            for obs in obstacles_to_move:
#                plan.obstacles.remove(obs)
#                plan.obstacles.add(process_box_movement(obs, movement))
#                plan.robot = coord


def run(plan: Plan, movements: str):
    for movement in movements:
        position = plan.robot
        coord = process_movement(position, movement)
        in_wall = coord in plan.walls
        obstacle = is_in_obstacle(coord, plan.obstacles)
        in_obstacle = obstacle is not None
        if not in_wall and not in_obstacle:
            plan.robot = coord

        if not in_wall and in_obstacle:
            obstacles_to_move = [obstacle]
            boxes = find_touching_boxes(obstacle, movement, plan.obstacles)
            while boxes:
                obstacles_to_move += boxes
                new_boxes = []
                for box in boxes:
                    new_boxes += find_touching_boxes(box, movement, plan.obstacles)
                boxes = new_boxes

            for box in obstacles_to_move:
                if is_box_touching_wall(box, movement, plan.walls):
                    obstacles_to_move = []

            if obstacles_to_move:
                obstacles_to_move = set(obstacles_to_move)
                new_obstacles = set()
                for obs in obstacles_to_move:
                    new_obstacles.add(process_box_movement(obs, movement))
                
                plan.obstacles = (plan.obstacles - obstacles_to_move).union(new_obstacles)
                plan.robot = coord


def box_gps(boxes: set[tuple[Point, Point]]) -> int:
    return sum([100 * b[0][1] + b[0][0] for b in boxes])


if __name__ == "__main__":
    filename = "input.txt"
    plan, movements = load_input(filename)
    run(plan, movements)
    print(f"Part 2: {box_gps(plan.obstacles)}")
