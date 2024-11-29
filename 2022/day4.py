with open("inputs/day4.txt") as f:
    data = f.read().splitlines()

def get_pair(elf):
    low, hi = elf.split(sep = "-")
    return (int(low), int(hi))

# Part 1
total = 0
pairs = []
for line in data:
    elf1, elf2 = line.split(sep = ',')
    pair1 = get_pair(elf1); pair2 = get_pair(elf2)
    pairs.append((pair1, pair2))
    if pair1[0] <= pair2[0] and pair1[1] >= pair2[1]:
        # Pair 1 subsumes pair 2
        total += 1
    elif pair1[0] >= pair2[0] and pair1[1] <= pair2[1]:
        # Pair 2 subsumes pair 1
        total += 1
    else:
        pass

print(total)

# Part 2
total = 0
for pair1, pair2 in pairs:
    if pair1[1] < pair2[0]:
        # Pair 1 falls below pair 2
        pass
    elif pair1[0] > pair2[1]:
        # Pair 1 falls above pair 2
        pass
    else:
        total += 1

print(total)