with open("inputs/day3.txt") as f:
    data = f.read().splitlines()

# Part one
def get_priority(item):
    if ord(item) < 97:
        # Capital letter
        return ord(item) - 38
    else:
        # Lowercase letter
        return ord(item) - 96

total = 0
for rucksack in data:
    # Split into two compartments of equal length
    c1 = rucksack[:len(rucksack)//2]
    c2 = rucksack[len(rucksack)//2:]
    # Find item in common
    item = (set(c1) & set(c2)).pop()
    # Add to running total
    total += get_priority(item)

print(total)

# Part two
total = 0
for i in range(0, len(data), 3):
    bags = data[i:i + 3]
    badge = (set(bags[0]) & set(bags[1]) & set(bags[2])).pop()
    total += get_priority(badge)

print(total)
