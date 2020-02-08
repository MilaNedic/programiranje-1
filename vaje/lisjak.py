from functools import lru_cache

test_matrix = [[2, 4, 1, 1], [3, 2, 0, 5], [8, 0, 7, 2]]

def fox(sadovnjak, max_koraki):
    @lru_cache(maxsize=None)
    def path(x, y, steps):
        st_vrstic = len(sadovnjak)
        st_stolpcev = len(sadovnjak[0])
        current_apples = sadovnjak[x][y]
        if steps == 0:
            return 0 
        elif x == st_vrstic - 1: #ne moremo dol
            if y == st_stolpcev - 1: #zadnje polje, to je najbolj desno spodaj, vedno koncamo
                return current_apples
            else:
                return current_apples + path(x, y + 1, steps - 1) #lahko se premaknemo desno
        else:
            if y == st_stolpcev - 1: # ne moremo desno, nujno v novo vrsto na zacetek, torej y = 0
                return current_apples + path(x + 1, 0, steps - 1)
            else: # lahko gremo desno ali dol v novo vrsto, gledamo max
                return current_apples + max(path(x, y + 1, steps - 1), path(x + 1, 0, steps - 1))
        
    return path(0, 0, max_koraki)

def fox_alt(sadovnjak, max_koraki):
    @lru_cache(maxsize=None)
    def path_alt(x, y, steps):
        st_vrstic = len(sadovnjak)
        st_stolpcev = len(sadovnjak[0])
        if x >= st_vrstic: # ne moremo dol
            return 0
        elif y >= st_stolpcev: # ne moremo desno
            return 0
        elif steps <= 0:
            return 0
        current_apples = sadovnjak[x][y]
        return current_apples + max(path_alt(x, y + 1, steps - 1), path_alt(x + 1, 0, steps - 1))
    
    return path_alt(0, 0, max_koraki)

print(fox_alt(test_matrix, 6))

test_cheese_matrix = [[1, 2, 0], [2, 4, 5], [7, 0, 1]]

def max_cheese(cheese_matrix):
    @lru_cache(maxsize=None)
    def path(x, y):
        st_vrstic = len(cheese_matrix)
        st_stolpcev = len(cheese_matrix[0])
        if x >= st_vrstic:
            return 0
        elif y >= st_stolpcev:
            return 0
        else:
            current_cheese = cheese_matrix[x][y]
        return current_cheese + max(path(x + 1, y), path(x, y + 1))

    return path(0, 0)

print(max_cheese(test_cheese_matrix))

def najdaljse_narascajoce_podzaporedje(sez):
    # Pomozna funkcija
    @lru_cache(maxsize=None)
    def najdaljse(spodnja_meja, i):
        # i označuje indeks trenutnega elementa
        if i >= len(sez):
            # Robni pogoj
            return []
        elif sez[i] < spodnja_meja:
            # Neprimeren element
            return najdaljse(spodnja_meja, i + 1)
        else:
            # Razvejitev in agregacija glede na dolzino
            z_prvim = [sez[i]] + najdaljse(sez[i], i + 1)
            brez_prvega = najdaljse(spodnja_meja, i + 1)
            if len(z_prvim) > len(brez_prvega):
                return z_prvim
            else:
                return brez_prvega
    # Zazenemo
    if len(sez) == 0:
        return []
    else:
        return najdaljse(sez[0], 0)