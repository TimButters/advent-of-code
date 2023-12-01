import re


print(sum([int(r[0] + r[-1]) for r in [re.findall("\d", l) for l in open("input.txt").readlines()]]))

d = {k: str(v+1) for k,v in zip(["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"], range(9))}
print(sum(int(r[0] + r[-1]) for r in [[d[i] if i in d else i for i in re.findall(f"(?=(\d|{'|'.join(d.keys())}))", l)] for l in open("input.txt").readlines()]))
