import sys

from itertools import permutations
from collections.abc import Collection


class Amplifier:
    def __init__(self, name: str, filename: str, phase: int):
        self.name = name
        self.program = self._load_data(filename)
        self.inputs = [phase]
        self.outputs = None

    def _load_data(self, filename: str) -> list[int]:
        with open(filename) as f:
            return [int(x) for x in f.readline().split(",")]

    def set_output_buffer(self, output: list[int]) -> None:
        self.outputs = output

    def run_program(self) -> int:
        pos = 0
        last_output = -99
        while True:
            instruction = [int(d) for d in str(self.program[pos])]
            opcode = instruction[-1]
            param_modes = instruction[-3::-1]

            if opcode == 9:
                return last_output

            if opcode == 1 or opcode == 2 or opcode == 7 or opcode == 8:
                increment = 4
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2, dest = self.program[pos + 1 : pos + 4]
                arg1, arg2 = [
                    a if p == 1 else self.program[a]
                    for a, p in zip([arg1, arg2], param_modes)
                ]
                if opcode == 1 or opcode == 2:
                    self.program[dest] = arg1 + arg2 if opcode == 1 else arg1 * arg2
                elif opcode == 7:
                    val = 1 if arg1 < arg2 else 0
                    self.program[dest] = val
                else:
                    val = 1 if arg1 == arg2 else 0
                    self.program[dest] = val
            elif opcode == 3:
                dest = self.program[pos + 1]
                self.program[dest] = self.inputs.pop()
                increment = 2
            elif opcode == 4:
                if len(param_modes) < 1:
                    param_modes = [0]
                dest = self.program[pos + 1]
                last_output = self.program[dest] if param_modes[0] == 0 else dest
                if self.outputs is not None:
                    self.outputs.insert(0, last_output)
                increment = 2
            elif opcode == 5 or opcode == 6:
                increment = 3
                arg1, arg2 = self.program[pos + 1 : pos + 3]
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2 = [
                    a if p == 1 else self.program[a]
                    for a, p in zip([arg1, arg2], param_modes)
                ]
                if (opcode == 5 and arg1 != 0) or (opcode == 6 and arg1 == 0):
                    pos = arg2
                    continue
            else:
                raise ValueError("Unexpected opcode", opcode)
            pos += increment


class Amplifiers:
    def __init__(self, filename: str, phases: Collection[int], feedback: bool = False):
        self.amplifiers = [Amplifier(name, filename, phase) for name, phase in zip(["A", "B", "C", "D", "E"], phases)]
        for i in range(len(self.amplifiers) - 1):
            self.amplifiers[i].set_output_buffer(self.amplifiers[i + 1].inputs)
        if feedback:
            self.amplifiers[-1].set_output_buffer(self.amplifiers[0].inputs)

    def run(self) -> int:
        output = 0
        self.amplifiers[0].inputs.insert(0, 0)
        for amp in self.amplifiers:
            output = amp.run_program()
        return output


def part1(filename: str):
    thrusters = []
    for phases in permutations([0, 1, 2, 3, 4]):
        amplifiers = Amplifiers(filename, phases)
        thrusters.append(amplifiers.run())
    return max(thrusters)


if __name__ == "__main__":
    filename = sys.argv[1]

    thrusters = part1(filename)
    print("Part 1:", thrusters)
