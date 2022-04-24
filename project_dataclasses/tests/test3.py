class List:
    pass

class Nil(List):
    pass

class Cons(List):
    car: int
    cdr: List

n = 0
ls = Nil()
while n < 10:
    ls = Cons(n, ls)
    n = n + 1

print(ls.car)
