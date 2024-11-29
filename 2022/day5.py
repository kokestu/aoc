with open("inputs/day5.txt") as f:
    data = f.read()
    stackstr, cmdstr = data.split(sep = "\n\n")

# Reverse crates?
part = 2

# Split and reorder stackstr, and drop junk column numbers row
stackstr = stackstr.splitlines()[:-1]
stackstr.reverse()

# Build stacks
nstacks = 1 + len(stackstr[0]) // 4
stacks = [[] for _ in range(nstacks)]
for row in stackstr:
    for i in range(nstacks):
        if row[1 + i*4] != " ":
            stacks[i].append(row[1 + i*4])

# Split cmd str
import re
cmdstr = re.sub("[^\d\n]+", " ", cmdstr)   # remove pointless words
cmdstr = cmdstr.split("\n")[:-1]

# Build commands
cmds = []
for line in cmdstr:
    ns = line.split(' ')
    cmds.append(
        # Cast to ints, and convert stack nums to indices
        (int(ns[1]), int(ns[2]) - 1, int(ns[3]) - 1)
    )

# Follow commands
for amount, out, into in cmds:
    # Remove from "out" stack
    crates = stacks[out][-amount:]
    del stacks[out][-amount:]
    # Add to "into" stack
    if part == 1:
        crates.reverse()
    stacks[into].extend(crates)

print("".join([(len(x) == 0 and " ") or x[-1] for x in stacks]))