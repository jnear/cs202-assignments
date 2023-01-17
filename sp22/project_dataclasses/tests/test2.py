class List:
    pass

class Nil(List):
    pass

class Cons(List):
    car: int
    cdr: List

ls = Cons(1, Cons(2, Nil()))
print(ls.car)
