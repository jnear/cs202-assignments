from dataclasses import dataclass
from cs202_support.base_ast import AST, RType
from typing import List, Set, Dict, Tuple

##################################################
# Types: Typed Rvec
##################################################

@dataclass
class RvecType(RType):
    pass

@dataclass
class IntT(RvecType):
    pass

@dataclass
class BoolT(RvecType):
    pass

@dataclass
class VoidT(RvecType):
    pass

@dataclass
class VectorT(RvecType):
    types: List[RvecType]


##################################################
# Abstract Syntax Trees: Typed RVec
##################################################

@dataclass
class RvecExpT(AST):
    pass

@dataclass
class IntTE(RvecExpT):
    val: int

@dataclass
class BoolTE(RvecExpT):
    val: bool

@dataclass
class VoidTE(RvecExpT):
    pass

@dataclass
class VarTE(RvecExpT):
    var: str
    typ: RvecType

@dataclass
class GlobalValTE(RvecExpT):
    var: str

@dataclass
class LetTE(RvecExpT):
    x: str
    e1: RvecExpT
    body: RvecExpT

@dataclass
class PrimTE(RvecExpT):
    op: str
    args: List[RvecExpT]
    typ: RvecType

@dataclass
class IfTE(RvecExpT):
    e1: RvecExpT
    e2: RvecExpT
    e3: RvecExpT
    typ: RvecType
