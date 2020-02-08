from functools import lru_cache

def simetricen(w):
    return w == w[::-1]

def vsotno_simetricen(w):
    if len(w) <= 1:
        return True
    l = [int(c) for c in w]
    n = int(len(l) / 2)
    return sum(l[:n]) == sum(l[n:])