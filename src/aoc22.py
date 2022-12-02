from typing import Callable, List, Tuple, Optional, Any
from itertools import zip_longest
from functools import reduce


##
## Utility functions
##
def main():
    for day in range(1, 26):
        for part in (1, 2):
            if f := globals().get(f"d{day}p{part}"):
                print(f"Result for day {day}, part {part}: {f()}")


def read_input_day(s: str, parse_line_fn: Optional[Callable] = None) -> List[Any]:
    """Read the filename 's'.txt from resources.

    It applies 'parse-line-fn' to all lines, if indicated"""

    fname = f"resources/aoc22/{s}.txt"
    with open(fname, "r") as f:
        ret = f.read().splitlines()
    if parse_line_fn:
        ret = list(map(parse_line_fn, ret))
    return ret


def opt_int(s: str) -> Optional[int]:
    if s.strip() == "":
        return None
    return int(s)


##
## Code
##

# Day 1
def calories(nums: List[Optional[int]]) -> List[int]:
    ret = []
    acc = 0
    for c in nums:
        if c == None:
            ret.append(acc)
            acc = 0
        else:
            acc += c
    if acc > 0:
        ret.append(acc)
    return ret


def position_max(nums: List[int]):
    return nums.index(max(nums)) + 1


def sum_top_n(n: int, nums: List[int]) -> int:
    ret = 0
    snums = set(nums)
    for i in range(n):
        m = max(snums)
        ret += m
        snums.remove(m)
    return ret


def test_d1():
    t = [
        1000,
        2000,
        3000,
        None,
        4000,
        None,
        5000,
        6000,
        None,
        7000,
        8000,
        9000,
        None,
        10000,
    ]
    assert position_max(calories(t)) == 4
    assert sum_top_n(3, calories(t)) == 45000
    assert d1p1() == 71934
    assert d1p2() == 211447


def d1p1():
    return max(calories(read_input_day("d1", opt_int)))


def d1p2():
    return sum_top_n(3, calories(read_input_day("d1", opt_int)))


# Day 2
values_option = {"A": 1, "B": 2, "C": 3}
equivalences = {"X": "A", "Y": "B", "Z": "C"}


def result_play(a, b) -> int:
    if a == b:
        return 1
    if a == "A":
        return 0 if b == "C" else 2
    if a == "B":
        return 0 if b == "A" else 2
    if a == "C":
        return 0 if b == "B" else 2


def score_play(p: Tuple) -> int:
    (play, reply) = p
    score_choice = 1
    if reply == "B":
        score_choice = 2
    if reply == "C":
        score_choice = 3
    return 3 * result_play(play, reply) + score_choice


def score_strategy(st: List) -> int:
    ret = 0
    for p in st:
        ret += score_play(p)
    return ret


def test_d2():
    assert score_strategy([("A", "B")]) == 8
    assert score_strategy([("B", "A")]) == 1
    assert score_strategy([("C", "C")]) == 6
    assert score_strategy([("A", "B"), ("B", "A"), ("C", "C")]) == 15
    assert d2p1() == 11767
    assert d2p2() == 13886


def parse_play(s: str) -> Tuple:
    (p, r) = tuple(s.split())
    r = equivalences[r]
    return (p, r)


winners = {"A": "B", "B": "C", "C": "A"}


def win_lose_to_choice(p: str, r: str) -> str:
    if r == "Y":
        return p
    if r == "Z":
        return winners[p]
    return next(k for k, v in winners.items() if v == p)


def parse_play2(s: str) -> Tuple:
    (p, r) = tuple(s.split())
    r = win_lose_to_choice(p, r)
    return (p, r)


def d2p1():
    return score_strategy(read_input_day("d2", parse_play))


def d2p2():
    return score_strategy(read_input_day("d2", parse_play2))


if __name__ == "__main__":
    main()
