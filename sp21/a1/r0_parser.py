from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST, print_ast
from lark import Lark

##################################################
# Abstract Syntax Trees: R0
##################################################

@dataclass
class R0Exp(AST):
    pass

@dataclass
class Int(R0Exp):
    val: int

##################################################
# Concrete Syntax Parser
##################################################

_r_var_parser = Lark(r"""
    ?exp: NUMBER -> int_e

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='exp')

##################################################
# Pass #0: Parsing Concrete to Abstract Syntax
##################################################

def _parse(s: str) -> R0Exp:
    def bast(e) -> R0Exp:
        if e.data == 'int_e':
            return Int(int(e.children[0]))
        else:
            raise Exception('parse', e)

    parsed = _r_var_parser.parse(s)
    ast = bast(parsed)
    return ast

def parse_rvar(s):
    return _parse(s)



