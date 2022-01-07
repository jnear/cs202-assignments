from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST, print_ast
from lark import Lark

##################################################
# Abstract Syntax Trees: Rvec
##################################################

@dataclass
class RvecExp(AST):
    pass

@dataclass
class Int(RvecExp):
    val: int

@dataclass
class Bool(RvecExp):
    val: bool

@dataclass
class Var(RvecExp):
    var: str

@dataclass
class Let(RvecExp):
    x: str
    e1: RvecExp
    body: RvecExp

@dataclass
class Prim(RvecExp):
    op: str
    args: List[RvecExp]

@dataclass
class If(RvecExp):
    e1: RvecExp
    e2: RvecExp
    e3: RvecExp


##################################################
# Concrete Syntax Parser
##################################################

_r_vec_parser = Lark(r"""
    ?exp: NUMBER -> int_e
        | "True" -> true_e
        | "False" -> false_e
        | CNAME -> var_e
        | "let" CNAME "=" exp "in" exp -> let_e
        | "if" exp "then" exp "else" exp -> if_e
        | "-" exp -> neg_e
        | "not" exp -> not_e
        | exp "+" exp -> plus_e
        | exp _cmp exp -> cmp_e
        | "vectorRef" "(" exp "," exp ")" -> vector_ref_e
        | "vectorSet" "(" exp "," exp "," exp ")" -> vector_set_e
        | "vector" "(" exp ("," exp)* ")" -> vector_e
        | "(" exp ")"

    !_cmp: "<"|">"|"=="|">="|"<="|"&&"|"||"

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='exp', parser='lalr')

##################################################
# Pass #0: Parsing Concrete to Abstract Syntax
##################################################

def _parse(s: str) -> RvecExp:
    def bast(e) -> RvecExp:
        if e.data == 'int_e':
            return Int(int(e.children[0]))
        if e.data == 'var_e':
            return Var(str(e.children[0]))
        elif e.data == 'plus_e':
            e1, e2 = e.children
            return Prim('+', [bast(e1), bast(e2)])
        elif e.data == 'let_e':
            x, e1, body = e.children
            return Let(str(x), bast(e1), bast(body))
        elif e.data == 'if_e':
            e1, e2, e3 = e.children
            return If(bast(e1), bast(e2), bast(e3))
        elif e.data == 'cmp_e':
            e1, op, e2 = e.children
            return Prim(str(op), [bast(e1), bast(e2)])
        elif e.data == 'not_e':
            e1 = e.children[0]
            return Prim('not', [bast(e1)])
        elif e.data == 'true_e':
            return Bool(True)
        elif e.data == 'false_e':
            return Bool(False)
        elif e.data == 'vector_ref_e':
            e1, e2 = e.children
            return Prim('vectorRef', [bast(e1), bast(e2)])
        elif e.data == 'vector_set_e':
            e1, e2, e3 = e.children
            return Prim('vectorSet', [bast(e1), bast(e2), bast(e3)])
        elif e.data == 'vector_e':
            new_children = [bast(c) for c in e.children]
            return Prim('vector', new_children)
        else:
            raise Exception('parse', e)

    parsed = _r_vec_parser.parse(s)
    ast = bast(parsed)
    return ast

def parse_rvec(s):
    return _parse(s)



