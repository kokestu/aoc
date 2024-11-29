# Part 1
from enum import Enum

class Turn(Enum):
    ROCK = 1
    PAPER = 2
    SCISSORS = 3

    def __lt__(self, other):
        if self.value == 1 and other.value == 3:
            return False
        elif self.value == 3 and other.value == 1:
            return True
        else:
            return self.value < other.value

    def score(self):
        return self.value

def get_turn(symbol):
    if symbol == 'A' or symbol == 'X':
        return Turn.ROCK
    elif symbol == 'B' or symbol == 'Y':
        return Turn.PAPER
    else:
        return Turn.SCISSORS

def score_round(them, me):
    total = 0
    # Score for contest
    if them == me:
        # Draw
        total += 3
    elif them < me:
        # Win
        total += 6
    else:
        # Lose
        pass
    # Score for choice
    total += me.score()
    return total

with open("inputs/day2.txt") as f:
    data = f.read().splitlines()

rounds = [(get_turn(line.split()[0]), get_turn(line.split()[1])) for line in data]
total = 0
for (them, me) in rounds:
    total += score_round(them, me)

print(total)

# Part 2
class Goal(Enum):
    LOSE = "X"
    DRAW = "Y"
    WIN = "Z"

def meet_goal(them, goal):
    if goal == Goal.WIN:
        if them == Turn.SCISSORS:
            return Turn.ROCK
        else:
            return Turn(them.value + 1)
    elif goal == Goal.DRAW:
        return them
    else:
        if them == Turn.ROCK:
            return Turn.SCISSORS
        else:
            return Turn(them.value - 1)


rounds = [(get_turn(line.split()[0]), Goal(line.split()[1])) for line in data]

total = 0
for (them, goal) in rounds:
    me = meet_goal(them, goal)
    total += score_round(them, me)

print(total)