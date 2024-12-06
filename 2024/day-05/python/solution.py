import networkx as nx


def load_input(filename: str):
    with open(filename) as f:
        content = f.read()
        rule_str, updates = content.split("\n\n")

        updates = [
                list(map(int, line.split(",")))
                for line in updates.strip().split("\n")
        ]
        rules = _build_rules(rule_str)
        graph = _build_rule_graph(rule_str)
    return rules, updates, graph


def _build_rules(rules: str) -> dict[int, list[int]]:
    ruledict = dict()
    for fst, lst in [map(int, r.split("|")) for r in rules.split("\n")]:
        if lst in ruledict:
            ruledict[lst].append(fst)
        else:
            ruledict[lst] = [fst]
    return ruledict


def _build_rule_graph(rules: str):
    G = nx.DiGraph()
    for fst, lst in [map(int, r.split("|")) for r in rules.split("\n")]:
        G.add_edge(fst, lst)
    return G


def find_valid_updates(rules, updates, invert=False):
    valid_updates = []
    for update in updates:
        valid = True
        for idx in range(len(update)):
            if update[idx] in rules and set(update[idx:]).intersection(rules[update[idx]]):
                valid = False
        if valid == (not invert):
            valid_updates.append(update)
    return valid_updates


def part1(rules, updates):
    return [update[len(update) // 2] for update in find_valid_updates(rules, updates)]


def part2(rules, updates, graph):
    invalid_updates = find_valid_updates(rules, updates, invert=True)
    valid_updates = []
    for update in invalid_updates:
        valid_order = list(nx.topological_sort(nx.subgraph(graph, update)))
        sorting = {valid_order.index(i): i for i in update}
        valid_update = [sorting[i] for i in sorted(sorting.keys())]
        valid_updates.append(valid_update[len(valid_update) // 2])
    return valid_updates


if __name__ == "__main__":
    filename = "input.txt"
    rules, updates, graph = load_input(filename)
    print("Part 1:", sum(part1(rules, updates)))
    print("Part 2:", sum(part2(rules, updates, graph)))
