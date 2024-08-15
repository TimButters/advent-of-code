import sys

from itertools import permutations
from collections.abc import Collection


def load_data(filename: str) -> list[int]:
    with open(filename) as f:
        return [int(x) for x in f.readline().split(",")]


def run_program(program: list[int], inputs: list[int]) -> int:
    pos = 0
    last_output = -99
    while True:
        instruction = [int(d) for d in str(program[pos])]
        opcode = instruction[-1]
        param_modes = instruction[-3::-1]

        if opcode == 9:
            return last_output

        if opcode == 1 or opcode == 2 or opcode == 7 or opcode == 8:
            increment = 4
            if len(param_modes) < 2:
                param_modes = param_modes + [0] * (2 - len(param_modes))
            arg1, arg2, dest = program[pos + 1 : pos + 4]
            arg1, arg2 = [
                a if p == 1 else program[a] for a, p in zip([arg1, arg2], param_modes)
            ]
            if opcode == 1 or opcode == 2:
                program[dest] = arg1 + arg2 if opcode == 1 else arg1 * arg2
            elif opcode == 7:
                val = 1 if arg1 < arg2 else 0
                program[dest] = val
            else:
                val = 1 if arg1 == arg2 else 0
                program[dest] = val
        elif opcode == 3:
            dest = program[pos + 1]
            program[dest] = inputs.pop()
            increment = 2
        elif opcode == 4:
            if len(param_modes) < 1:
                param_modes = [0]
            dest = program[pos + 1]
            last_output = program[dest] if param_modes[0] == 0 else dest
            increment = 2
        elif opcode == 5 or opcode == 6:
            increment = 3
            arg1, arg2 = program[pos + 1 : pos + 3]
            if len(param_modes) < 2:
                param_modes = param_modes + [0] * (2 - len(param_modes))
            arg1, arg2 = [
                a if p == 1 else program[a] for a, p in zip([arg1, arg2], param_modes)
            ]
            if (opcode == 5 and arg1 != 0) or (opcode == 6 and arg1 == 0):
                pos = arg2
                continue
        else:
            raise ValueError("Unexpected opcode", opcode)
        pos += increment


def run_amplifiers(filename: str, phases: Collection[int]) -> int:
    amplifiers = [load_data(filename) for _ in range(5)]
    output = 0
    for amp, phase in zip(amplifiers, phases):
        output = run_program(amp, inputs=[output, phase])
    return output


def part1(filename: str):
    return max(
        [run_amplifiers(filename, phases) for phases in permutations([0, 1, 2, 3, 4])]
    )


if __name__ == "__main__":
    filename = sys.argv[1]

    thrusters = part1(filename)
    print("Part 1:", thrusters)
