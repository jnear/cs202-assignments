from dataclasses import dataclass, fields
from typing import List, Tuple

# ==================================================
# AST Definition
# ==================================================

class AST:
    pass

class RType(AST):
    pass

class Expr(AST):
    pass

class Stmt(AST):
    pass

@dataclass(frozen=True, eq=True)
class Program(AST):
    stmts: List[Stmt]

# ==================================================
# statements

@dataclass(frozen=True, eq=True)
class Print(Stmt):
    arg: Expr

@dataclass(frozen=True, eq=True)
class Return(Stmt):
    arg: Expr

@dataclass(frozen=True, eq=True)
class Assign(Stmt):
    name: str
    rhs: Expr

@dataclass(frozen=True, eq=True)
class If(Stmt):
    condition: Expr
    then_stmts: List[Stmt]
    else_stmts: List[Stmt]

@dataclass(frozen=True, eq=True)
class While(Stmt):
    condition: Expr
    body: List[Stmt]

@dataclass(frozen=True, eq=True)
class FunctionDef(Stmt):
    name: str
    params: List[Tuple[str, type]]
    body: List[Stmt]
    return_type: type

@dataclass(frozen=True, eq=True)
class ClassDef(Stmt):
    name: str
    superclass: str
    body: List[Stmt]

# ==================================================
# expressions

@dataclass(frozen=True, eq=True)
class Prim(Expr):
    op: str
    args: List[Expr]

@dataclass(frozen=True, eq=True)
class Constant(Expr):
    val: any

@dataclass(frozen=True, eq=True)
class Var(Expr):
    name: str

@dataclass(frozen=True, eq=True)
class Begin(Expr):
    stmts: List[Stmt]
    exp: Expr

@dataclass(frozen=True, eq=True)
class Call(Expr):
    function: Expr
    args: List[Expr]

@dataclass(frozen=True, eq=True)
class FieldRef(Expr):
    lhs: Expr
    field: str
