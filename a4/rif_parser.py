from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST, print_ast
from lark import Lark

##################################################
# Abstract Syntax Trees: Rif
##################################################

@dataclass
class RifExp(AST):
    pass

@dataclass
class Int(RifExp):
    val: int

@dataclass
class Bool(RifExp):
    val: bool

@dataclass
class Var(RifExp):
    var: str

@dataclass
class Let(RifExp):
    x: str
    e1: RifExp
    body: RifExp

@dataclass
class Prim(RifExp):
    op: str
    args: List[RifExp]

@dataclass
class If(RifExp):
    e1: RifExp
    e2: RifExp
    e3: RifExp


##################################################
# Concrete Syntax Parser
##################################################

_r_var_parser = Lark(r"""
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

def _parse(s: str) -> RifExp:
    def bast(e) -> RifExp:
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
        else:
            raise Exception('parse', e)

    parsed = _r_var_parser.parse(s)
    ast = bast(parsed)
    return ast

def parse_rif(s):
    return _parse(s)



