from collections import Counter
from functools import cmp_to_key


def load_input(filename: str):
    with open(filename) as f:
        return [
            (left, int(right))
            for left, right in [line.strip().split() for line in f.readlines()]
        ]


def order_hands(hand1, hand2, part2=False):
    h1, _ = hand1
    h2, _ = hand2
    if part2:
        score_1 = classify_hand2(h1)
        score_2 = classify_hand2(h2)
    else:
        score_1 = classify_hand(h1)
        score_2 = classify_hand(h2)
    if score_1 == score_2:
        return order_high_cards(h1, h2, part2)
    return 1 if score_1 > score_2 else -1


def order_high_cards(h1, h2, part2=False):
    card_order = ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]
    if part2:
        card_order = ["A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"]
    for c1, c2 in zip(h1, h2):
        if c1 == c2:
            continue
        return 1 if card_order.index(c1) < card_order.index(c2) else -1


def classify_hand2(h):
    if "J" not in h:
        return classify_hand(h)

    count = Counter(h)
    counts = sorted([i for _, i in count.items()], reverse=True)
    if counts == [5] or len(counts) == 2:
        return 7  # Five of a kind
    elif counts == [3, 1, 1] or (counts == [2, 2, 1] and ("J", 2) in count.items()):
        return 6
    elif counts == [2, 2, 1]:
        return 5
    elif counts == [2, 1, 1, 1]:
        return 4
    elif counts == [1, 1, 1, 1, 1]:
        return 2
    else:
        raise ValueError("Joker should mean min of 1 pair")


def classify_hand(h):
    count = Counter(h)
    counts = sorted([i for _, i in count.items()], reverse=True)
    if counts == [5]:
        return 7  # Five of a kind
    elif counts == [4, 1]:
        return 6
    elif counts == [3, 2]:
        return 5
    elif counts == [3, 1, 1]:
        return 4
    elif counts == [2, 2, 1]:
        return 3
    elif counts == [2, 1, 1, 1]:
        return 2
    else:
        return 1


if __name__ == "__main__":
    filename = "input.txt"
    hands = load_input(filename)
    sorted_hands = sorted(hands, key=cmp_to_key(order_hands))
    print(sum([rank * bid for rank, (_, bid) in enumerate(sorted_hands, start=1)]))

    sorted_hands2 = sorted(hands, key=cmp_to_key(lambda x, y: order_hands(x, y, True)))
    print(sum([rank * bid for rank, (_, bid) in enumerate(sorted_hands2, start=1)]))
