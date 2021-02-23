from dataclasses import dataclass, fields

class AST:
    pass


def print_ast(obj, indent=0):
    if isinstance(obj, AST):
        name = type(obj).__name__
        flds = [getattr(obj, f.name) for f in fields(obj)]
        indentation = ' ' * indent

        if len(flds) <= 1:
            children = ''.join([print_ast(f_v, indent=0) for f_v in flds])
            return indentation + f'{name}({children})'
        else:
            children = ',\n'.join([print_ast(f_v, indent=indent+1) for f_v in flds])
            return indentation + f'{name}(\n{children})'

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

    elif isinstance(obj, (str, int)):
        return ' ' * indent + str(obj)

    else:
        return str(obj)


