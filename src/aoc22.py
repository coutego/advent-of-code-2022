from typing import Callable, List, Optional, Any
from itertools import zip_longest
from functools import reduce


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


def num_increases(nums: List) -> int:
    ret = 0
    for i in range(0, len(nums) - 1):
        if nums[i] < nums[i + 1]:
            ret += 1
    return ret


def three_window_sum(nums: List[int]) -> List[int]:
    return list(map(lambda x, y, z: x + y + z, nums, nums[1:], nums[2:]))


def test_d1():
    t = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    assert num_increases(t) == 7
    assert num_increases(three_window_sum(t)) == 5
    assert d1p1() == 1722
    assert d1p2() == 1748


def d1p1():
    return num_increases(read_input_day("d1p1", int))


def d1p2():
    return num_increases(three_window_sum(read_input_day("d1p1", int)))


if __name__ == "__main__":
    main()
