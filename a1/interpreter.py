import ast
from ast import *

def eval_Lmin(program: Module) -> int:
    match program:
        case Module([Expr(Call(Name('print'), [Constant(i)]))]):
            return i

