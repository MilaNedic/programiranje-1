from functools import lru_cache

# n je sirina korita
# m je stevilo korit
# l je sirina korita

test = [0,0,0,0,0,0,0,0,0]

def rozice(n, m, l):
    @lru_cache(maxsize=None)
    # ce je dolzina korita daljse od balkona, ga ne mormeo postaviti na balkon
    if l > n:
        return None
    elif l == n:
        return 1
    else:
        # ce skupna dolzina korit presega dolzino balkona, mormao zagotovo enega odstraniti
        if m * l > n:
            return rozice(n, m - 1, l)

