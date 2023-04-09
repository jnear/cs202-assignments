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
class Call(Expr):
    func: Atm
    args: List[Atm]

@dataclass(frozen=True, eq=True)
class Allocate(Expr):
    num_bytes: int
    tuple_type: any

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
class Return(Stmt):
    exp: Atm

@dataclass(frozen=True, eq=True)
class Collect(Stmt):
    num_bytes: int

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
class CFunctionDef(AST):
    name: str
    args: List[str]
    blocks: Dict[str, List[Stmt]]

@dataclass(frozen=True, eq=True)
class CProgram(AST):
    defs: List[CFunctionDef]

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
            case Call(fun, args):
                fun_str = print_exp(fun)
                args_str = ', '.join([print_exp(a) for a in args])
                return f'{fun_str}({args_str})'
            case _:
                raise Exception('unknown expression:', e)

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
            case _:
                raise Exception('unknown statement:', s)


    output_lines = ''
    for fundef in program.defs:
        arg_names = ', '.join(fundef.args)
        output_lines += f'def {fundef.name}({arg_names}):\n'
        for name, stmts in fundef.blocks.items():
            output_lines += f'  {name}:\n'
            for s in stmts:
                output_lines += '    ' + print_stmt(s) + '\n'
        output_lines += '\n'
            
    return output_lines
