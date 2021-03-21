from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST, print_ast
from lark import Lark

##################################################
# Abstract Syntax Trees: RVar
##################################################

@dataclass
class RVarExp(AST):
    pass

@dataclass
class Int(RVarExp):
    val: int

@dataclass
class Var(RVarExp):
    var: str

@dataclass
class Let(RVarExp):
    x: str
    e1: RVarExp
    body: RVarExp

@dataclass
class Prim(RVarExp):
    op: str
    args: List[RVarExp]

##################################################
# Concrete Syntax Parser
##################################################

_r_var_parser = Lark(r"""
    ?exp: NUMBER -> int_e
        | CNAME -> var_e
        | "let" CNAME "=" exp "in" exp -> let_e
        | "-" exp -> neg_e
        | exp "+" exp -> plus_e
        | "(" exp ")"

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='exp')

##################################################
# Pass #0: Parsing Concrete to Abstract Syntax
##################################################

def _parse(s: str) -> RVarExp:
    def bast(e) -> RVarExp:
        if e.data == 'int_e':
            return Int(int(e.children[0]))
        if e.data == 'var_e':
            return Var(str(e.children[0]))
        elif e.data == 'plus_e':
            e1, e2 = e.children
            return Prim('+', [bast(e1), bast(e2)])
        elif e.data == 'let_e':
            x, e1, body = e.children
            return Let(x, bast(e1), bast(body))
        else:
            raise Exception('parse', e)

    parsed = _r_var_parser.parse(s)
    ast = bast(parsed)
    return ast

def parse_rvar(s):
    return _parse(s)



