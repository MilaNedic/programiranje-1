from functools import lru_cache

def frog_escape(swamp):
    @lru_cache(maxsize=None)
    def jump(position, e):
        if position >= len(swamp): #rebni pogoj: ce je zacetna pozicija >= enaka dolzini mocvare, je zabica uspesno pobegnila
            return 0
        else:
            e += swamp[position] #se postavimo na dano pozicijo in dobimo swamp[position] energije, to je toliko muh, kot se nahaja na danem polju
            return 1 + min([jump(position + d, e - d) for d in range(1, e + 1)]) #seznam vrednosti useh moznih skokov
            #porabimo en skos plus minimum vseh moznosti, ko skocimo za d naprej, pri cemer d med 1 in e
            #obe meji vkljuceni, range(1, e+1) da [1, e+1)
            #pri tem se energija zmanjsa za d, trenutna pozicija pa gre na position + d
    return jump(0, 0) #zacnemo na zacetku z 0 energije


test1 = [2, 4, 1, 2, 1, 3, 1, 1, 5]
test2 = [4, 1, 8, 2, 11, 1, 1, 1, 1, 1]