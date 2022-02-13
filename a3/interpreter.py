from ast import *
from typing import List, Set, Dict, Tuple

def eval_Lvar(prog: Module) -> List[int]:
    def eval_stmts(stmts: List[stmt]) -> List[int]:
        outputs = []
        env = {}
        for stmt in stmts:
            match stmt:
                case Assign([Name(x)], e):
                    env[x] = eval_e(e, env)
                case Expr(Call(Name('print'), [e])):
                    outputs.append(eval_e(e, env))
                case _:
                    raise Exception('eval_stmts', dump(stmt))
        return outputs
        
    def eval_e(e: expr, env: Dict[str, int]) -> int:
        match e:
            case Constant(i):
                return i
            case Name(x):
                return env[x]
            case BinOp(e1, Add(), e2):
                return eval_e(e1, env) + eval_e(e2, env)
            case _:
                raise Exception('eval_e', dump(e))

    match prog:
        case Module(stmts):
            return eval_stmts(stmts)
        case _:
            raise Exception('eval_Lvar', prog)
