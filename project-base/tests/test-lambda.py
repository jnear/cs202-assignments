x = 5

def f() -> int:
    return x

x = 6

def g(a: int) -> int:
    y = 20
    def h(z: int) -> int:
        return x + y + z + a
    y = 0
    a = 3
    return h(a)

print(f())
print(g(2))
