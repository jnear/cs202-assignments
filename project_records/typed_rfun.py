from dataclasses import dataclass
from cs202_support.base_ast import AST, RType
from typing import List, Set, Dict, Tuple

##################################################
# Types: Typed Rvec
##################################################

@dataclass
class RfunType(RType):
    pass

@dataclass
class IntT(RfunType):
    pass

@dataclass
class BoolT(RfunType):
    pass

@dataclass
class VoidT(RfunType):
    pass

@dataclass
class VectorT(RfunType):
    types: List[RfunType]

@dataclass
class FunT(RfunType):
    arg_types: List[RfunType]
    return_type: RfunType

@dataclass
class RecordT(RfunType):
    name: str


##################################################
# Abstract Syntax Trees: Typed RVec
##################################################

@dataclass
class RfunExpT(AST):
    pass

@dataclass
class IntTE(RfunExpT):
    val: int

@dataclass
class BoolTE(RfunExpT):
    val: bool

@dataclass
class VoidTE(RfunExpT):
    pass

@dataclass
class VarTE(RfunExpT):
    var: str
    typ: RfunType

@dataclass
class GlobalValTE(RfunExpT):
    var: str

@dataclass
class LetTE(RfunExpT):
    x: str
    e1: RfunExpT
    body: RfunExpT

@dataclass
class PrimTE(RfunExpT):
    op: str
    args: List[RfunExpT]
    typ: RfunType

@dataclass
class IfTE(RfunExpT):
    e1: RfunExpT
    e2: RfunExpT
    e3: RfunExpT
    typ: RfunType

@dataclass
class FuncallTE(RfunExpT):
    fun: RfunExpT
    args: List[RfunExpT]
    typ: RfunType

@dataclass
class FunRefTE(RfunExpT):
    name: str
    typ: FunT

@dataclass
class FieldRefTE(RfunExpT):
    e1: RfunExpT
    field: str
    typ: RfunType

@dataclass
class RfunDefT(AST):
    name: str
    args: List[Tuple[str, RfunType]]
    output_type: RfunType
    body: RfunExpT

@dataclass
class RfunRecordDefT(AST):
    name: str
    fields: List[Tuple[str, RfunType]]

@dataclass
class RfunProgramT(AST):
    record_defs: List[RfunRecordDefT]
    defs: List[RfunDefT]
    body: RfunExpT
