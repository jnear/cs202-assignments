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

def eval_rif(e: RifExp):
    def eval_e(e: RifExp, env: Dict[str, int]):
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
