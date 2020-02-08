from functools import lru_cache

def korita(n, m, l):
    if n == 0:
        return 0
    if l > n:            
        return 0
    counter = 0
    if m * (l + 1) <= n:
        counter += 1 + korita(n - m, m - 1, l)
    return counter

print(korita(9, 3, 2))