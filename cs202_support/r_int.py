from dataclasses import dataclass
from .base_ast import AST

# Concrete syntax
# exp ::= int | (read) | (- exp) | (+ exp exp)
# R Int ::= exp

from lark import Lark
_r0_parser = Lark(r"""
    ?exp: NUMBER -> int_e
        | exp "+" exp -> plus_e
        | "(" exp ")"

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='exp')


# Abstract syntax
# exp ::= (Int int) | (Prim read ()) | (Prim - (exp))
# | (Prim + (exp exp))
# R Int ::= (Program ’() exp)

@dataclass
class RIntExp(AST):
    pass

@dataclass
class Int(RIntExp):
    val: int

@dataclass
class Plus(RIntExp):
    e1: RIntExp
    e2: RIntExp

def parse(s):
    def bast(e):
        if e.data == 'int_e':
            return Int(int(e.children[0]))
        elif e.data == 'plus_e':
            e1, e2 = e.children
            return Plus(bast(e1), bast(e2))

    parsed = _r0_parser.parse(s)
    ast = bast(parsed)
    return ast

if __name__ == '__main__':
    e = Plus(Int(5), Int(6))
    print('A simple expression:', e.pprint())

    stest = '1 + (2 + 3)'
    print('A parsed expression:', parse(stest).pprint())
