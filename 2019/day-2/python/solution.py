import sys
from copy import deepcopy


def load_data(filename: str) -> list[int]:
    with open(filename) as f:
        return [int(x) for x in f.readline().split(",")]


def run_program(program: list[int], noun=12, verb=2) -> int:
    program[1] = noun
    program[2] = verb

    pos = 0
    while True:
        opcode = program[pos]
        if opcode == 99:
            return program[0]

        arg1, arg2, dest = program[pos+1: pos+4]
        if opcode == 1:
            program[dest] = program[arg1] + program[arg2]
        elif opcode == 2:
            program[dest] = program[arg1] * program[arg2]
        else:
            raise ValueError("Unexpected opcode", opcode)
        pos += 4


def part2(program: list[int]) -> int:
    for noun in range(0, 100):
        for verb in range(0, 100):
            p = deepcopy(program)
            run_program(p, noun, verb)

            if p[0] == 19690720:
                return 100 * noun + verb
    return -1


if __name__ == "__main__":
    filename = "../input.txt" if len(sys.argv) < 2 else sys.argv[1]
    data = load_data(filename)
    program = deepcopy(data)

    print("Part 1:", run_program(program))
    print("Part 2:", part2(data))
