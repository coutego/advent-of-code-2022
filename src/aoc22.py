from typing import Callable, List, Tuple, Optional, Any
from itertools import zip_longest
from functools import reduce
import re


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
# Rock, Paper, Scissors is better modeled as values module 3. This allows us to
# just use mod aritmethic to calculate results. One more always wins and 2 + 1 = 0 (mod 3)
# so 0 wins to 2.
plays = ["A", "B", "C", "X", "Y", "Z"]


def play2n(p: str) -> int:
    return plays.index(p) % 3


def result_play(p1: str, p2: str) -> int:
    return (play2n(p2) - play2n(p1) + 1) % 3


def score_play(p1: str, p2: str) -> int:
    return 1 + play2n(p2) + 3 * result_play(p1, p2)


def score_strategy(st: List) -> int:
    ret = 0
    for (a, b) in st:
        ret += score_play(a, b)
    return ret


def parse_play(s: str) -> Tuple:
    return tuple(s.split())


def parse_play_win_lose(s: str) -> Tuple:
    (p, r) = parse_play(s)
    idx = (play2n(p) - 1 + play2n(r)) % 3
    r = plays[idx]
    return (p, r)


def d2p1():
    return score_strategy(read_input_day("d2", parse_play))


def d2p2():
    return score_strategy(read_input_day("d2", parse_play_win_lose))


def test_d2():
    assert score_strategy([("A", "B")]) == 8
    assert score_strategy([("B", "A")]) == 1
    assert score_strategy([("C", "C")]) == 6
    assert score_strategy([("A", "B"), ("B", "A"), ("C", "C")]) == 15
    assert d2p1() == 11767
    assert d2p2() == 13886


# Day 4
Range = Tuple[int, int]


def fully_contained(r1: Range, r2: Range) -> bool:
    return r1[0] >= r2[0] and r1[1] <= r2[1]


def fully_overlap(r1: Range, r2: Range) -> bool:
    return fully_contained(r1, r2) or fully_contained(r2, r1)


def in_range(r1: Range, n: int) -> bool:
    (a, b) = r1
    return a <= n and n <= b


def range_in_range(r1: Range, r2: Range) -> bool:
    return in_range(r1, r2[0]) or in_range(r1, r2[1])


def overlap(r1: Range, r2: Range) -> bool:
    return range_in_range(r1, r2) or range_in_range(r2, r1)


def parse_d4(line: str) -> Any:
    v = re.split("[,-]", line)
    v = list(map(int, v))
    return [[v[0], v[1]], [v[2], v[3]]]


def d4p1() -> int:
    return len(
        [True for (r1, r2) in read_input_day("d4", parse_d4) if fully_overlap(r1, r2)]
    )


def d4p2() -> int:
    return len([True for (r1, r2) in read_input_day("d4", parse_d4) if overlap(r1, r2)])


def test_d4():
    assert 644 == d4p1()
    assert 926 == d4p2()


if __name__ == "__main__":
    main()
