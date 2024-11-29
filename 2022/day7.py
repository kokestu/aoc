with open("inputs/day7.txt") as f:
    data = f.read().splitlines()

class File:
    def __init__(self, name, size):
        self.size = size
        self.name = name
    
    def get_size(self):
        return self.size

    def build_sizes(self, _, _):
        pass
        
    def __repr__(self):
        return f"{self.size} {self.name}"
    
    def __hash__(self):
        return hash(self.name)

class Dir:
    def __init__(self, name, parent):
        self.size = None
        self.children = set()
        self.name = name
        self.parent = parent

    def add_child(self, child):
        self.children.add(child)

    def get_size(self):
        if self.size:
            return self.size
        total = 0
        for child in self.children:
            total += child.get_size()
        self.size = total
        return total
    
    def build_sizes(self, sizes, check):
        if check(self.get_size()):
            sizes.append(self.size)
        for child in self.children:
            child.build_sizes(sizes, check)
    
    def __repr__(self):
        return f"dir {self.name}"

    def __hash__(self):
        return hash(self.name)

## Build the tree ##
root = Dir("/", None)
current = root

# First line is cd /
data = data[1:]
for line in data:
    if line == "$ cd ..":
        # Go to the parent
        current = current.parent
    elif line.startswith("$ cd"):
        # Change current pointer
        target = line.split(sep = " ")[2]
        for child in current.children:
            if child.name == target:
                current = child
                break
        else:
            raise RuntimeError(f"No child called {target} in {current.name}!")
    elif line == "$ ls":
        # We can ignore these
        pass
    else:
        # Add children to current pointer
        info, name = line.split(sep = " ")
        if info == "dir":
            current.add_child(Dir(name, current))
        else:
            # The info is the size
            current.add_child(File(name, int(info)))

# Part one
sizecap = 100000
sizes = []
root.build_sizes(sizes, lambda size: size < sizecap)
sum(sizes)

# Part two
total_space = 70000000
necessary_free = 30000000
current_used = root.get_size()
to_delete = current_used + necessary_free - total_space

sizes = []
root.build_sizes(sizes, lambda size: size > to_delete)
min(sizes)