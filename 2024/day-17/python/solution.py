class Computer:
    def __init__(self, a: int, b: int, c: int, program: list[int]):
        self.A = a
        self.B = b
        self.C = c
        self.program = program
        self.ip = 0
        self.output = []

    @staticmethod
    def lowest_3bit(num):
        if num < 8:
            return num
        return int(bin(num)[-3:], 2)

    def combo_operand(self, op):
        if op < 4:
            return op
        elif op == 4:
            return self.A
        elif op == 5:
            return self.B
        elif op == 6:
            return self.C
        else:
            raise ValueError(f"Invalid combo operand {op}")

    def opcode(self, op, operand):
        match op:
            case 0:
                self.ip += 2
                self.A = int(self.A / 2 ** self.combo_operand(operand))
            case 1:
                self.ip += 2
                self.B = self.B ^ operand
            case 2:
                self.ip += 2
                self.B = Computer.lowest_3bit(self.combo_operand(operand) % 8)
            case 3:
                self.ip += 2
                if self.A != 0:
                    self.ip = operand
            case 4:
                self.ip += 2
                self.B = self.B ^ self.C
            case 5:
                self.ip += 2
                self.output.append(str(self.combo_operand(operand) % 8))
            case 6:
                self.ip += 2
                self.B = int(self.A / 2 ** self.combo_operand(operand))
            case 7:
                self.ip += 2
                self.C = int(self.A / 2 ** self.combo_operand(operand))
            case _:
                raise ValueError(f"Invalid Opcode {op}")

    def run(self):
        while self.ip < len(self.program):
            op, operand = self.program[self.ip : self.ip + 2]
            self.opcode(op, operand)

    def run_match(self):
        prog = list(map(str, self.program))
        while self.ip < len(self.program):
            op, operand = self.program[self.ip : self.ip + 2]
            self.opcode(op, operand)
            if self.output and (self.output != prog[0 : len(self.output)]):
                return False
        return True if len(self.output) == len(prog) else False


def load_input(filename: str) -> list[int]:
    with open(filename) as f:
        initials, prog = f.read().split("\n\n")
        prog = prog[9:].strip()

    initials = [int(line.strip()[12:]) for line in initials.split("\n")]
    program = list(map(int, prog.split(",")))
    return Computer(*initials, program)


def part2(computer: Computer, init_b, init_c):
    a = 0
    while True:
        computer.ip = 0
        computer.A = a
        computer.B = init_b
        computer.C = init_c
        computer.output = []
        match = computer.run_match()
        if match:
            return a
        a += 1
    return None


if __name__ == "__main__":
    filename = "input.txt"
    computer = load_input(filename)
    init_b = computer.B
    init_c = computer.C
    computer.run()
    output = ",".join(computer.output)
    print(f"Part 1: {output}")
    print(f"Part 2: {part2(computer, init_b, init_c)}")
