from dataclasses import dataclass
from typing import List, Set, Dict, Tuple
from cs202_support.python import AST

##################################################
@dataclass(frozen=True, eq=True)
class Expr(AST):
    pass

@dataclass(frozen=True, eq=True)
class Atm(Expr):
    pass

@dataclass(frozen=True, eq=True)
class Prim(Expr):
    op: str
    args: List[Atm]

@dataclass(frozen=True, eq=True)
class Constant(Atm):
    val: any

@dataclass(frozen=True, eq=True)
class Var(Atm):
    var: str

##################################################
@dataclass(frozen=True, eq=True)
class Stmt(AST):
    pass

@dataclass(frozen=True, eq=True)
class Print(Stmt):
    arg: Atm

@dataclass(frozen=True, eq=True)
class Assign(Stmt):
    var: str
    exp: Expr

@dataclass(frozen=True, eq=True)
class Return(Stmt):
    exp: Atm

@dataclass(frozen=True, eq=True)
class Goto(Stmt):
    label: str

@dataclass(frozen=True, eq=True)
class If(Stmt):
    test: Expr
    then_branch: Goto
    else_branch: Goto

##################################################
@dataclass(frozen=True, eq=True)
class CProgram(AST):
    blocks: Dict[str, List[Stmt]]


def print_program(program: CProgram):
    def print_exp(e: Expr):
        match e:
            case Constant(c):
                return str(c)
            case Var(x):
                return x
            case Prim(op, args):
                args_str = ', '.join([print_exp(a) for a in args])
                return f'{op}({args_str})'

    def print_stmt(s: Stmt):
        match s:
            case Print(a):
                return f'print({print_exp(a)})'
            case Assign(x, e):
                return f'{x} = {print_exp(e)}'
            case Return(e):
                return f'return {print_exp(e)}'
            case Goto(l):
                return f'goto {l}'
            case If(condition, then_branch, else_branch):
                return f'if {print_exp(condition)}: goto {then_branch.label} else: goto {else_branch.label}'

    output_lines = ''
    for name, stmts in program.blocks.items():
        output_lines += f'{name}:\n'
        for s in stmts:
            output_lines += '  ' + print_stmt(s) + '\n'

    return output_lines
