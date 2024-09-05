import sys
import numpy as np
import matplotlib.pyplot as plt


directions = {}


class IntCode:
    def __init__(self, filename: str, start_block_white=False):
        self.program = self._load_data(filename) + [0] * 10000
        self.outputs = []
        self.relative_base = 0
        self.pos = 0
        self.grid = {(0, 0): 1} if start_block_white else {}
        self.location = (0, 0)
        self.direction = 0

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
                return len(self.grid)

            if opcode == 1 or opcode == 2 or opcode == 7 or opcode == 8:
                increment = 4
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2, dest = self.program[self.pos + 1 : self.pos + 4]
                arg1, arg2 = [
                    (
                        a
                        if p == 1
                        else (
                            self.program[a]
                            if p == 0
                            else self.program[self.relative_base + a]
                        )
                    )
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
                dest = self.program[self.pos + 1]
                if param_modes and param_modes[0] == 2:
                    dest += self.relative_base

                if not self.location in self.grid:
                    self.program[dest] = 0
                else:
                    self.program[dest] = self.grid[self.location]

                increment = 2
            elif opcode == 4:
                increment = 2
                if len(param_modes) < 1:
                    param_modes = [0]
                dest = self.program[self.pos + 1]
                if param_modes[0] == 0:
                    last_output = self.program[dest]
                elif param_modes[0] == 1:
                    last_output = dest
                else:
                    last_output = self.program[self.relative_base + dest]

                self.outputs.append(last_output)
                if len(self.outputs) == 2:
                    colour, direction = self.outputs
                    self.outputs = []
                    self.grid[self.location] = colour
                    direction = -1 if direction == 0 else direction
                    self.direction = (self.direction + direction) % 4
                    x, y = self.location
                    if self.direction == 0:
                        self.location = (x, y + 1)
                    elif self.direction == 1:
                        self.location = (x + 1, y)
                    elif self.direction == 2:
                        self.location = (x, y - 1)
                    elif self.direction == 3:
                        self.location = (x - 1, y)
                    else:
                        raise ValueError("Unknown direction.")
            elif opcode == 5 or opcode == 6:
                increment = 3
                arg1, arg2 = self.program[self.pos + 1 : self.pos + 3]
                if len(param_modes) < 2:
                    param_modes = param_modes + [0] * (2 - len(param_modes))
                arg1, arg2 = [
                    (
                        a
                        if p == 1
                        else (
                            self.program[a]
                            if p == 0
                            else self.program[self.relative_base + a]
                        )
                    )
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

    comp1 = IntCode(filename)
    hull = comp1.run_program()
    print("Part 1:", hull)

    comp2 = IntCode(filename, start_block_white=True)
    _ = comp2.run_program()

    whites = [coord for coord, colour in comp2.grid.items() if colour == 1]
    min_x = min(whites, key=lambda c: c[0])[0]
    max_x = max(whites, key=lambda c: c[0])[0]
    min_y = min(whites, key=lambda c: c[1])[1]
    max_y = max(whites, key=lambda c: c[1])[1]

    norm_whites = [(x - min_x, y - min_y) for x, y in whites]

    image = np.zeros((max_x - min_x + 1, max_y - min_y + 1))
    for x, y in norm_whites:
        image[x, y] = 1

    plt.imshow(image.T, origin="lower")
    plt.show()
