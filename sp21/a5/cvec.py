from dataclasses import dataclass
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST
import typed_rvec

@dataclass
class Atm(AST):
    pass

@dataclass
class Int(Atm):
    val: int

@dataclass
class Bool(Atm):
    val: bool

@dataclass
class Void(Atm):
    pass

@dataclass
class Var(Atm):
    var: str
    typ: typed_rvec.RvecType

@dataclass
class GlobalVal(Atm):
    val: str

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
    typ: typed_rvec.RvecType

@dataclass
class Stmt(AST):
    pass

@dataclass
class Assign(Stmt):
    var: str
    exp: Exp
    is_vec: bool

@dataclass
class Collect(Stmt):
    amount: int

@dataclass
class Tail(AST):
    pass

@dataclass
class Return(Tail):
    exp: Exp

@dataclass
class Goto(Tail):
    label: str

@dataclass
class If(Tail):
    test: Exp
    then_label: str
    else_label: str

@dataclass
class Seq(Tail):
    stmt: Stmt
    tail: Tail

@dataclass
class Program(AST):
    blocks: Dict[str, Tail]
