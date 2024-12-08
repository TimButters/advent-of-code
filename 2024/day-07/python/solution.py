Sum = tuple[int, list[int]]


class Node:
    def __init__(self, value: int, level: int):
        self.value = value
        self.level = level
        self.left = None
        self.right = None
        self.middle = None


class BinaryTree:
    def __init__(self, root: int, part2=False):
        self.nodes = [Node(root, 0)]
        self.level = 0
        self.part2 = part2

    def insert_num(self, value: int):
        for node in self.nodes:
            if node.level == self.level:
                node.left = Node(node.value + value, self.level + 1)
                node.right = Node(node.value * value, self.level + 1)
                self.nodes.append(node.left)
                self.nodes.append(node.right)
                if self.part2:
                    node.middle = Node(int(str(node.value) + str(value)), self.level + 1)
                    self.nodes.append(node.middle)
        self.level += 1

    def get_leaves(self) -> list[int]:
        return [node.value for node in self.nodes if node.level == self.level]


def load_input(filename: str) -> list[Sum]:
    lines = []
    with open(filename) as f:
        for line in f.readlines():
            target, nums = line.strip().split(":")
            nums = [int(num) for num in nums.strip().split(" ")]
            lines.append((int(target), nums))
    return lines


def find_valid_sums(sums: list[Sum], part2=False) -> int:
    valid_sums = 0
    for sum in sums:
        target, nums = sum
        tree = BinaryTree(nums[0], part2=part2)
        for num in nums[1:]:
            tree.insert_num(num)
        answers = tree.get_leaves()
        if target in answers:
            valid_sums += target
    return valid_sums


if __name__ == "__main__":
    filename = "input.txt"
    sums = load_input(filename)
    print(f"Part 1: {find_valid_sums(sums)}")
    print(f"Part 2: {find_valid_sums(sums, True)}")
