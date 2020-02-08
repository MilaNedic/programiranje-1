from functools import lru_cache

zabojniki = [1, 3, 4, 7, 10]
#max_kapaciteta = 300

def natovarjanje(zabojniki, kapaciteta):
    @lru_cache(maxsize=None)
    def prestej_moznosti(k, cap): #k oznacuje mesto seznama zabojniki, torej s tezo katerega zabojnika smo zaceli
        if cap == 0:
            return 1 #edina moznost je zabojnik z 0 teze
        dim = len(zabojniki)
        if k >= dim: #smo izven mej vseh moznih zabojnikov
            return 0
        counter = 0
        teza = zabojniki[k]
        if teza <= cap:
            # naprej porestejemo vse moznosti za dano tezo
            counter += prestej_moznosti(k, cap - teza) #porabili smo ze za tezo zabojnika celotne kapacitete ladje
        # dodamo se vse oznosti, ce zacnemo z drugo tezo
        counter += prestej_moznosti(k + 1, cap) #premaknemo se na naslednji zabojnik, pogledmao vse moznosti, ce zacnemo z enim vecjim itd
        return counter
    return prestej_moznosti(0, kapaciteta)

print(natovarjanje(zabojniki, 5))
print(natovarjanje(zabojniki, 40))
print(natovarjanje(zabojniki, 300))