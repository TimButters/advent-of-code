import sys


class IntCode:
    def __init__(self, filename: str, phase: int):
        self.program = self._load_data(filename) + [0] * 10000
        self.inputs = [phase]
        self.outputs = None
        self.relative_base = 0
        self.pos = 0

    def _load_data(self, filename: str) -> list[int]:
        with open(filename) as f:
            return [int(x) for x in f.readline().split(",")]

    def set_output_buffer(self, output: list[int]) -> None:
        self.outputs = output

    def run_program(self) -> int | None:
        last_output = -99
        while True:
            instruction = [int(d) for d in str(self.program[self.pos])]
            opcode = instruction[-1]
            param_modes = instruction[-3::-1]

            if len(instruction) > 1 and instruction[-2] == 9 and instruction[-1] == 9:
                return last_output

            if opcode == 1 or opcode == 2 or opcode == 7 or opcode == 8:
                increment = 4
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2, dest = self.program[self.pos + 1 : self.pos + 4]
                arg1, arg2 = [
                    a if p == 1 else self.program[a] if p == 0 else self.program[self.relative_base + a]
                    for a, p in zip([arg1, arg2], param_modes)
                ]
                if len(param_modes) > 2 and param_modes[2] == 2:
                    dest += self.relative_base
                if opcode == 1 or opcode == 2:
                    self.program[dest] = arg1 + arg2 if opcode == 1 else arg1 * arg2
                elif opcode == 7:
                    val = 1 if arg1 < arg2 else 0
                    self.program[dest] = val
                else:
                    val = 1 if arg1 == arg2 else 0
                    self.program[dest] = val
            elif opcode == 3:
                if not self.inputs:
                    print("break")
                    break
                dest = self.program[self.pos + 1]
                if param_modes and param_modes[0] == 2:
                    dest += self.relative_base
                self.program[dest] = self.inputs.pop()
                increment = 2
            elif opcode == 4:
                if len(param_modes) < 1:
                    param_modes = [0]
                dest = self.program[self.pos + 1]
                if param_modes[0] == 0:
                    last_output = self.program[dest]
                elif param_modes[0] == 1:
                    last_output = dest
                else:
                    last_output = self.program[self.relative_base + dest]
                if self.outputs is not None:
                    self.outputs.insert(0, last_output)
                else:
                    print(last_output)
                increment = 2
            elif opcode == 5 or opcode == 6:
                increment = 3
                arg1, arg2 = self.program[self.pos + 1 : self.pos + 3]
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2 = [
                    a if p == 1 else self.program[a] if p == 0 else self.program[self.relative_base + a]
                    for a, p in zip([arg1, arg2], param_modes)
                ]
                if (opcode == 5 and arg1 != 0) or (opcode == 6 and arg1 == 0):
                    self.pos = arg2
                    continue
            elif opcode == 9:
                if len(param_modes) < 1:
                    param_modes = [0]
                dest = self.program[self.pos + 1]
                if param_modes[0] == 0:
                    offset = self.program[dest]
                elif param_modes[0] == 1:
                    offset = dest
                else:
                    offset = self.program[self.relative_base + dest]
                self.relative_base += offset
                increment = 2
            else:
                raise ValueError("Unexpected opcode", opcode)
            self.pos += increment


if __name__ == "__main__":
    filename = sys.argv[1]

    comp = IntCode(filename, 1)
    keycode = comp.run_program()
    print("Part 1:", keycode)

    comp = IntCode(filename, 2)
    coords = comp.run_program()
    print("Part 2:", coords)
