from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Any

from compiler import *

binops = {
    '+': lambda a, b: a + b,
    '==': lambda a, b: a == b,
    '&&': lambda a, b: a and b,
    '||': lambda a, b: a or b,
    '>': lambda a, b: a > b,
    '<': lambda a, b: a < b,
    '>=': lambda a, b: a >= b,
    '<=': lambda a, b: a <= b,
    }

unops = {
    'not': lambda a: not a
    }

@dataclass
class VectorVal:
    values: List[Any]

def eval_rvec(e: RvecExp):
    def eval_e(e: RvecExp, env: Dict[str, Any]):
        if isinstance(e, Int):
            return e.val
        if isinstance(e, Bool):
            return e.val
        elif isinstance(e, Var):
            return env[e.var]
        elif isinstance(e, Let):
            new_env = {**env, e.x: eval_e(e.e1, env)}
            return eval_e(e.body, new_env)
        elif isinstance(e, Prim):
            if e.op in binops:
                e1, e2 = e.args
                f = binops[e.op]
                return f(eval_e(e1, env), eval_e(e2, env))
            elif e.op in unops:
                e1 = e.args[0]
                f = unops[e.op]
                return f(eval_e(e1, env))
            elif e.op == 'vector':
                vals = [eval_e(a, env) for a in e.args]
                return VectorVal(vals)
            elif e.op == 'vectorRef':
                e1, idx = e.args
                v1 = eval_e(e1, env)
                assert isinstance(v1, VectorVal)
                assert isinstance(idx, Int)

                return v1.values[idx.val]
            elif e.op == 'vectorSet':
                e1, idx, e2 = e.args
                v1 = eval_e(e1, env)
                v2 = eval_e(e2, env)
                assert isinstance(v1, VectorVal)
                assert isinstance(idx, Int)
                v1.values[idx.val] = v2

                return 0
            else:
                raise Exception('eval_e: unknown primitive: ', e)
        elif isinstance(e, If):
            if eval_e(e.e1, env):
                return eval_e(e.e2, env)
            else:
                return eval_e(e.e3, env)
        else:
            raise Exception('eval_e', e)

    return eval_e(e, {})
