from compiler import *

def eval_rvar(e: R0Exp) -> int:
    def eval_e(e: R0Exp, env: Dict[str, int]) -> int:
        if isinstance(e, Int):
            return e.val
        else:
            raise Exception('eval_e', e)

    return eval_e(e, {})
