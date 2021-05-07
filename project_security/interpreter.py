from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Any

from compiler import *

def unzip2(ls):
    if ls == []:
        return [], []
    else:
        xs, ys = zip(*ls)
        return list(xs), list(ys)

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
    'not': lambda a: not a,
    'neg': lambda a: (- a)
    }

@dataclass
class VectorVal:
    values: List[Any]

@dataclass
class FunVal:
    name: str
    args: List[str]
    env: Dict[str, Any]
    body: RfunExp

@dataclass
class RecordConstructorVal:
    name: str
    fields: List[str]

@dataclass
class RecordVal:
    name: str
    fields: Dict[str, any]

def eval_rfun(program: RfunProgram):
    def eval_e(e: RfunExp, env: Dict[str, Any]):
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
        elif isinstance(e, Funcall):
            f_val = eval_e(e.fun, env)
            arg_vals = [eval_e(a, env) for a in e.args]

            if isinstance(f_val, FunVal):
                extended_env = f_val.env.copy()
                for arg_name, arg_val in zip(f_val.args, arg_vals):
                    extended_env[arg_name] = arg_val

                return eval_e(f_val.body, extended_env)

            elif isinstance(f_val, RecordConstructorVal):
                return RecordVal(f_val.name, dict(zip(f_val.fields, arg_vals)))

            else:
                raise Exception('eval_e', e)

        elif isinstance(e, FieldRef):
            v1 = eval_e(e.e1, env)
            return v1.fields[e.field]

        else:
            raise Exception('eval_e', e)

    env = {}

    # add constructors for records to the initial environment
    for d in program.record_defs:
        field_names, field_types = unzip2(d.fields)
        env[d.name] = RecordConstructorVal(d.name, list(field_names))

    # add top-level functions to the initial environment
    for d in program.defs:
        arg_names, arg_types = unzip2(d.args)
        env[d.name] = FunVal(d.name,
                             list(arg_names),
                             env,
                             d.body)

    return eval_e(program.body, env)
