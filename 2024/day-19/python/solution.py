def load_input(filename: str):
    with open(filename) as f:
        towels, patterns = f.read().split("\n\n")
    towels = [towel.strip() for towel in towels.split(",")]
    patterns = [pattern.strip() for pattern in patterns.strip().split("\n")]
    return towels, patterns


def check_pattern(pattern, towels):
    matches = [(towel, len(towel)) for towel in towels if towel == pattern[0:len(towel)]]
    while matches:
        new_matches = []
        for _, match_len in matches:
            for towel in towels:
                if towel == pattern[match_len: match_len+len(towel)]:
                    match = (towel, match_len + len(towel))
                    new_matches.append(match)
                    if len(pattern) == match_len + len(towel):
                        return True
        matches = list(set(new_matches))
    return False
        

if __name__ == "__main__":
    filename = "input.txt"
    towels, patterns = load_input(filename)
    num_possible = sum([check_pattern(pattern, towels) for pattern in patterns])
    print(f"Part 1: {num_possible}")
