from compiler import *

def eval_rvar(e: RVarExp) -> int:
    def eval_e(e: RVarExp, env: Dict[str, int]) -> int:
        if isinstance(e, Int):
            return e.val
        elif isinstance(e, Var):
            return env[e.var]
        elif isinstance(e, Let):
            new_env = {**env, e.x: eval_e(e.e1, env)}
            return eval_e(e.body, new_env)
        elif isinstance(e, Prim):
            if e.op == '+':
                e1, e2 = e.args
                return eval_e(e1, env) + eval_e(e2, env)
            else:
                raise Exception('eval_e', e)
        else:
            raise Exception('eval_e', e)

    return eval_e(e, {})
