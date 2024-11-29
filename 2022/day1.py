# Part 1
with open("inputs/day1.txt") as f:
    data = f.read().splitlines()

answer = 0
current = 0
for line in data:
    if line == '':
        answer = max(answer, current)
        current = 0
    else:
        current += int(line)

print(answer)

# Part 2
answers = []
current = 0
for line in data:
    if line == '':
        answers.append(current)
        current = 0
    else:
        current += int(line)

print(sum(sorted(answers)[-3:]))
