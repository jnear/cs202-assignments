from dataclasses import dataclass
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST

@dataclass
class Atm(AST):
    pass

@dataclass
class Int(Atm):
    val: int

@dataclass
class Var(Atm):
    var: str

@dataclass
class Exp(AST):
    pass

@dataclass
class AtmExp(Exp):
    atm: Atm

@dataclass
class Prim(Exp):
    op: str
    args: List[Atm]

@dataclass
class Stmt(AST):
    pass

@dataclass
class Assign(Stmt):
    var: str
    exp: Exp

@dataclass
class Tail(AST):
    pass

@dataclass
class Return(Tail):
    exp: Exp

@dataclass
class Seq(Tail):
    stmt: Stmt
    tail: Tail

@dataclass
class Program(AST):
    blocks: Dict[str, Tail]
