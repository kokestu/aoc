with open("inputs/day6.txt") as f:
    data = f.read()

# Part one
def do_thing(size):
    for i in range(size, len(data)):
        window = data[i-size:i]
        if len(set(window)) == size:
            return i

print(do_thing(4))

# Part two
print(do_thing(14))