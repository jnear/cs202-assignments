import ast
from .python_ast import *

# ==================================================
# Pretty printer (abstract syntax)
# ==================================================

def print_type(obj, depth=0):
    if depth > 5:
        return '...'
    elif isinstance(obj, RType):
        name = type(obj).__name__
        flds = [getattr(obj, f.name) for f in fields(obj)]
        children = ', '.join([print_type(f_v, depth=depth+1) for f_v in flds])
        if len(children) > 80:
            return f'{name}(...)'
        else:
            return f'{name}({children})'
    elif isinstance(obj, list):
        return '[' + ', '.join([print_type(a) for a in obj]) + ']'
    else:
        return str(obj)

def print_ast(obj, indent=0):
    if isinstance(obj, ast.AST):
        name = type(obj).__name__
        flds = [f for n, f in ast.iter_fields(obj)]
        indentation = ' ' * indent

        if len(flds) == 0:
            return indentation + f'{name}()'
        elif len(flds) == 1 and isinstance(flds[0], (str, int)):
            children = ''.join([print_ast(f_v, indent=0) for f_v in flds])
            return indentation + f'{name}({children})'
        else:
            children = ',\n'.join([print_ast(f_v, indent=indent+1) for f_v in flds])
            return indentation + f'{name}(\n{children})'

    elif isinstance(obj, AST):
        name = type(obj).__name__
        flds = [getattr(obj, f.name) for f in fields(obj)]
        indentation = ' ' * indent

        if len(flds) == 0:
            return indentation + f'{name}()'
        elif len(flds) == 1 and isinstance(flds[0], (str, int)):
            children = ''.join([print_ast(f_v, indent=0) for f_v in flds])
            return indentation + f'{name}({children})'
        else:
            children = ',\n'.join([print_ast(f_v, indent=indent+1) for f_v in flds])
            return indentation + f'{name}(\n{children})'

    elif isinstance(obj, RType):
        indentation = ' ' * indent
        return indentation + print_type(obj)

    elif isinstance(obj, list):
        if len(obj) == 0:
            return ' ' * indent + '[]'
        else:
            children = ',\n'.join([print_ast(e, indent=indent+1) for e in obj])
            return ' ' * indent + '[\n' + children + '\n' + ' ' * indent + ']'

    elif isinstance(obj, set):
        if len(obj) == 0:
            return ' ' * indent + '{}'
        else:
            children = ',\n'.join([print_ast(e, indent=indent+1) for e in obj])
            return ' ' * indent + '{\n' + children + '\n' + ' ' * indent + '}'

    elif isinstance(obj, tuple):
        first, *rest = obj
        first_str = print_ast(first, indent=0) + ',\n'
        rest_str = ',\n'.join([print_ast(e, indent=indent+1) for e in rest])
        return ' ' * indent + '(' + first_str + rest_str + ')'

    elif isinstance(obj, dict):
        keys = [' ' * (indent+1) + "'" + k + "':" for k in obj.keys()]
        val_strs = [print_ast(v, indent=indent+2) for v in obj.values()]
        all_strs = [k + '\n' + v for k, v in zip(keys, val_strs)]

        final_result = ' ' * indent + '{\n' + '\n'.join(all_strs) + '\n' + ' ' * indent + '}'
        return final_result

    elif isinstance(obj, int):
        return ' ' * indent + str(obj)
    elif isinstance(obj, str):
        return ' ' * indent + '"' + str(obj) + '"'

    else:
        return ' ' * indent + str(obj)
