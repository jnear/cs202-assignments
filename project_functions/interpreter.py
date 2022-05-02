from ast import *
from typing import List, Set, Dict
import functools
from dataclasses import dataclass

binops = {
    '+': lambda a, b: a + b,
    'Eq': lambda a, b: a == b,
    'And': lambda a, b: a and b,
    'Or': lambda a, b: a or b,
    'Gt': lambda a, b: a > b,
    'Lt': lambda a, b: a < b,
    'GtE': lambda a, b: a >= b,
    'LtE': lambda a, b: a <= b,
    }

unops = {
    'Not': lambda a: not a
    }

@dataclass
class FunVal:
    args: List[str]
    body: List[stmt]

def eval_Lif(prog: Module) -> List[int]:
    outputs = []

    def eval_stmts(stmts: List[stmt], env: Dict[str, any]) -> List[any]:
        for stmt in stmts:
            match stmt:
                case Return(e):
                    return eval_e(e, env)
                case FunctionDef(name, args, body):
                    arg_names = [a.arg for a in args.args]
                    env[name] = FunVal(arg_names, body)

                case Assign([Name(x)], e):
                    env[x] = eval_e(e, env)
                case Expr(Call(Name('print'), [e])):
                    outputs.append(eval_e(e, env))
                case If(condition, then_stmts, else_stmts):
                    if eval_e(condition, env):
                        return eval_stmts(then_stmts, env)
                    else:
                        return eval_stmts(else_stmts, env)
                case While(test, body):
                    while eval_e(test, env):
                        eval_stmts(body, env)
                case _:
                    raise Exception('eval_stmts', dump(stmt))
        
    def eval_e(e: expr, env: Dict[str, any]) -> any:
        match e:
            case Call(Name(fun_name), args):
                fv = env[fun_name]
                arg_vals = [eval_e(a, env) for a in args]
                new_env = env.copy()
                for a, v in zip(fv.args, arg_vals):
                    new_env[a] = v
                retval = eval_stmts(fv.body, new_env)
                return retval

            case Constant(i):
                return i
            case Name(x):
                return env[x]
            case UnaryOp(op, e1):
                return unops[type(op).__name__](eval_e(e1, env))
            case BinOp(e1, Add(), e2):
                return eval_e(e1, env) + eval_e(e2, env)
            case BinOp(e1, Sub(), e2):
                return eval_e(e1, env) - eval_e(e2, env)
            case BinOp(e1, Mult(), e2):
                return eval_e(e1, env) * eval_e(e2, env)
            case Compare(e1, [op], [e2]):
                return binops[type(op).__name__](eval_e(e1, env), eval_e(e2, env))
            case BoolOp(op, args):
                vals = [eval_e(e, env) for e in args]
                return functools.reduce(binops[type(op).__name__], vals)
            case IfExp(test, then_e, else_e):
                if eval_e(test, env):
                    return eval_e(then_e, env)
                else:
                    return eval_e(else_e, env)
            case Tuple(args):
                return tuple([eval_e(a, env) for a in args])
            case Subscript(e1, e2):
                return eval_e(e1, env)[eval_e(e2, env)]
            case _:
                raise Exception('eval_e', dump(e))

    env = {}
    match prog:
        case Module(stmts):
            eval_stmts(stmts, env)
            return outputs
        case _:
            raise Exception('eval_Lif', prog)
