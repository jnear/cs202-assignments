from .python_ast import *
import ast

# ==================================================
# Parser & AST translator
# ==================================================

def parse(s):
    # Returns the "simple" name of an object's type
    def name_of(op):
        return type(op).__name__.lower()

    # turns a type annotation into a type
    def get_type(a):
        match a:
            case ast.Name('int'):
                return int
            case ast.Name('bool'):
                return bool
            case ast.Tuple(args):
                return tuple([get_type(a) for a in args])
            case ast.Name(x):
                return x
            case _:
                raise Exception('get_type', ast.dump(a))

    def trans_prog(e):
        match e:
            case ast.Module(stmts):
                return Program(trans_stmts(stmts))
            case _:
                print(print_ast(e))
                raise Exception('trans_prog', e)
    def trans_stmts(stmts):
        return [trans_stmt(s) for s in stmts]
    def trans_classdef(decl):
        match decl:
            case ast.AnnAssign(ast.Name(x), t, _, _):
                return (x, get_type(t))
            case _:
                raise Exception('trans_classdef', decl)
                
    def trans_stmt(s):
        match s:
            case ast.Expr(ast.Call(ast.Name('print'), [e])):
                return Print(trans_expr(e))
            case ast.Assign([ast.Name(n)], e):
                return Assign(n, trans_expr(e))
            case ast.If(e1, stmts1, stmts2):
                return If(trans_expr(e1),
                          trans_stmts(stmts1),
                          trans_stmts(stmts2))
            case ast.While(condition, stmts, []):
                return While(trans_expr(condition), trans_stmts(stmts))
            case ast.Return(e1):
                return Return(trans_expr(e1))
            case ast.FunctionDef(name, args, stmts, _, typ, _):
                new_args = [(a.arg, get_type(a.annotation)) for a in args.args]
                new_stmts = trans_stmts(stmts)
                return FunctionDef(name, new_args, new_stmts, get_type(typ))
            case ast.ClassDef(name, [], [], [ast.Pass()], []):
                return ClassDef(name, None, [])
            case ast.ClassDef(name, [], [], decls, []):
                new_decls = [] if ast.Pass() in decls else [trans_classdef(d) for d in decls]
                return ClassDef(name, None, new_decls)
            case ast.ClassDef(name, [ast.Name(x)], [], [ast.Pass()], []):
                return ClassDef(name, x, [])
            case ast.ClassDef(name, [ast.Name(x)], [], decls, []):
                new_decls = [] if ast.Pass() in decls else [trans_classdef(d) for d in decls]
                return ClassDef(name, x, new_decls)
            case _:
                print(print_ast(s))
                raise Exception('trans_stmt', s)
    def trans_expr(e):
        match e:
            case ast.Compare(e1, [op], [e2]):
                return Prim(name_of(op), [trans_expr(e1), trans_expr(e2)])
            case ast.BoolOp(op, args):
                e1, e2 = args
                return Prim(name_of(op), [trans_expr(e1), trans_expr(e2)])
            case ast.UnaryOp(op, e1):
                return Prim(name_of(op), [trans_expr(e1)])
            case ast.Call(ast.Name('print'), [e]):
                return Prim('print', [trans_expr(e)])
            case ast.Constant(c):
                assert isinstance(c, (bool, int))
                return Constant(c)
            case ast.BinOp(e1, ast.Add(), e2):
                return Prim('add', [trans_expr(e1), trans_expr(e2)])
            case ast.BinOp(e1, ast.Mult(), e2):
                return Prim('mult', [trans_expr(e1), trans_expr(e2)])
            case ast.BinOp(e1, ast.Sub(), e2):
                return Prim('sub', [trans_expr(e1), trans_expr(e2)])
            case ast.Tuple(args):
                return Prim('tuple', [trans_expr(a) for a in args])
            case ast.Subscript(e1, e2):
                return Prim('subscript', [trans_expr(e1), trans_expr(e2)])
            case ast.Name(n):
                return Var(n)
            case ast.Call(e1, args):
                return Call(trans_expr(e1), [trans_expr(a) for a in args])
            case ast.Attribute(e1, f):
                return FieldRef(trans_expr(e1), f)
            case _:
                print(print_ast(e))
                raise Exception('trans_expr', e)

    python_ast = ast.parse(s)
    return trans_prog(python_ast)
