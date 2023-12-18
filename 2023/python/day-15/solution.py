from functools import reduce
import re


def load_input(filename: str):
    with open(filename) as f:
        return f.read().strip().split(",")


def hash_value(current_value, c):
    return ((current_value + ord(c)) * 17) % 256


def hash(input):
    return [reduce(hash_value, inp, 0) for inp in input]


class LensLibrary:
    def __init__(self):
        self.boxes = {n: dict() for n in range(256)}

    def process_operation(self, operation: str):
        label, operator, value = re.match(r"^(\w+)(-|=)(\d)?$", operation).groups()
        box = hash([label])[0]
        if operator == "=":
            self.boxes[box][label] = int(value)
        elif operator == "-":
            if label in self.boxes[box]:
                self.boxes[box].pop(label)
        else:
            raise ValueError(f"Unrecognised operator `{operator}`")
        return self

    def run(self, operatrions):
        [self.process_operation(operation) for operation in operatrions]
        return self

    def power(self):
        focussing_power = 0
        for box, lenses in self.boxes.items():
            focussing_power += sum([(1 + box) * pos * lens for pos, lens in enumerate(lenses.values(), start=1)])
        return focussing_power


if __name__ == "__main__":
    filename = "input.txt"
    print(sum(hash(load_input(filename))))

    lib = LensLibrary()
    lib.run(load_input(filename))
    print(lib.power())
