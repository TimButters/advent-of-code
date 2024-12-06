Xword = list[list[str]]


def load_input(filename: str) -> Xword:
    with open(filename) as f:
        return [list(line.strip()) for line in f.readlines()]


def transpose(xword: Xword) -> Xword:
    t = []
    M = len(xword)
    N = len(xword[0])
    for i in range(N):
        t_row = []
        for j in range(M):
            t_row.append(xword[j][i])
        t.append(t_row)
    return t


def diagonals(xword: Xword) -> Xword:
    d = []
    M = len(xword)
    N = len(xword[0])

    for j in range(N):
        d_row = []
        for i in range(M):
            if j - i < 0:
                break
            d_row.append(xword[i][j-i])
        d.append(d_row)

    for i in range(M-1, -1, -1):
        d_row = []
        for j in range(N-1, -1, -1):
            if i + (N - j) >= M:
                break
            d_row.append(xword[i + (N-j)][j])
        if d_row:
            d.append(d_row)

    return d


def count_xmas(xword: Xword) -> int:
    XMAS = list("XMAS")
    count = 0

    for row in xword:
        search_idx = 0
        for char in row:
            if char != XMAS[search_idx]:
                search_idx = 1 if char == XMAS[0] else 0
                continue
            search_idx += 1
            if search_idx == len(XMAS):
                search_idx = 0
                count += 1
    return count


def part1(xword: Xword) -> int:
    fward = count_xmas(xword)
    bward = count_xmas([line[::-1] for line in xword])
    down = count_xmas(transpose(xword))
    up = count_xmas([line[::-1] for line in transpose(xword)])
    diag1 = count_xmas(diagonals(xword))
    diag1_rev = count_xmas([line[::-1] for line in diagonals(xword)])
    diag2 = count_xmas(diagonals([line[::-1] for line in xword]))
    diag2_rev = count_xmas([row[::-1] for row in diagonals([line[::-1] for line in xword])])
    return fward + bward + down + up + diag1 + diag1_rev + diag2 + diag2_rev


def part2(xword: Xword) -> int:
    M = len(xword)
    N = len(xword[0])

    count = 0
    for i in range(1, M-1):
        for j in range(1, N-1):
            if xword[i][j] == "A":
                if (len(set([xword[i-1][j-1], xword[i][j], xword[i+1][j+1]]).intersection("MAS")) == 3
                    and len(set([xword[i-1][j+1], xword[i][j], xword[i+1][j-1]]).intersection("MAS")) == 3):

                    count += 1
    return count


if __name__ == "__main__":
    filename = "input.txt"
    xword = load_input(filename)
    print(part1(xword))
    print(part2(xword))
