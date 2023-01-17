from dataclasses import dataclass
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST

##################################################
@dataclass(frozen=True, eq=True)
class Atm(AST):
    pass

@dataclass(frozen=True, eq=True)
class ConstantInt(Atm):
    val: int

@dataclass(frozen=True, eq=True)
class ConstantBool(Atm):
    val: bool

@dataclass(frozen=True, eq=True)
class Name(Atm):
    var: str

##################################################
@dataclass(frozen=True, eq=True)
class Exp(AST):
    pass

@dataclass(frozen=True, eq=True)
class AtmExp(Exp):
    atm: Atm

@dataclass(frozen=True, eq=True)
class BinOp(Exp):
    e1: Exp
    op: any
    e2: Exp

@dataclass(frozen=True, eq=True)
class UnaryOp(Exp):
    op: any
    e1: Exp

@dataclass(frozen=True, eq=True)
class Compare(Exp):
    e1: Exp
    op: any
    e2: Exp

##################################################
@dataclass(frozen=True, eq=True)
class Stmt(AST):
    pass

@dataclass(frozen=True, eq=True)
class Print(Stmt):
    exp: Exp

@dataclass(frozen=True, eq=True)
class Assign(Stmt):
    var: str
    exp: Exp

@dataclass(frozen=True, eq=True)
class Return(Stmt):
    exp: Exp

@dataclass(frozen=True, eq=True)
class Goto(Stmt):
    label: str

@dataclass(frozen=True, eq=True)
class If(Stmt):
    test: Compare
    then_branch: Goto
    else_branch: Goto

##################################################
@dataclass(frozen=True, eq=True)
class CProgram(AST):
    blocks: Dict[str, List[Stmt]]
