import re


def load_input(filename: str):
    def process_part(s):
        return {p.split("=")[0]: int(p.split("=")[1]) for p in s[1:-1].split(",")}

    with open(filename) as f:
        input = f.read()

    workflows_raw, parts_raw = input.split("\n\n")
    workflows = {flow[0:flow.find("{")]: flow[flow.find("{")+1:-1] for flow in workflows_raw.split("\n")}
    parts = [process_part(part) for part in parts_raw.split("\n") if part.strip()]
    return workflows, parts


def process_workflow(rules, part):
    for rule in rules.split(","):
        if re.match(r"^\w+$", rule):
            return rule

        field, op, val, dest = re.match(r"(\w+)(<|>)(\d+):(\w+)", rule).groups()
        result = eval(f"{part[field]} {op} {val}")
        if result:
            return dest
    raise ValueError(f"Something went wrong for {rules}, {part}")


if __name__ == "__main__":
    filename = "input.txt"
    workflows, parts = load_input(filename)

    accepted = []
    for part in parts:
        wflow = "in"
        while wflow not in ["A", "R"]:
            wflow = process_workflow(workflows[wflow], part)
            if wflow == "A":
                accepted.append(part)
    print(sum([sum(a.values()) for a in accepted]))
