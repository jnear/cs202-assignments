from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from cs202_support.base_ast import AST, print_ast
from lark import Lark
from typed_rfun import *

##################################################
# Abstract Syntax Trees: Rvec
##################################################

@dataclass
class RfunExp(AST):
    pass

@dataclass
class Int(RfunExp):
    val: int

@dataclass
class Bool(RfunExp):
    val: bool

@dataclass
class Var(RfunExp):
    var: str

@dataclass
class Let(RfunExp):
    x: str
    e1: RfunExp
    body: RfunExp

@dataclass
class Prim(RfunExp):
    op: str
    args: List[RfunExp]

@dataclass
class If(RfunExp):
    e1: RfunExp
    e2: RfunExp
    e3: RfunExp

@dataclass
class Funcall(RfunExp):
    fun: RfunExp
    args: List[RfunExp]

@dataclass
class FieldRef(RfunExp):
    e1: RfunExp
    field: str

@dataclass
class RfunDef(AST):
    name: str
    args: List[Tuple[str, RfunType]]
    output_type: RfunType
    body: RfunExp

@dataclass
class RfunRecordDef(AST):
    name: str
    fields: List[Tuple[str, RfunType]]

@dataclass
class RfunProgram(AST):
    high_security_variables: List[str]
    record_defs: List[RfunRecordDef]
    defs: List[RfunDef]
    body: RfunExp


##################################################
# Concrete Syntax Parser
##################################################

_rfun_parser = Lark(r"""
    ?exp: NUMBER -> int_e
        | "True" -> true_e
        | "False" -> false_e
        | CNAME -> var_e
        | "let" CNAME "=" exp "in" exp -> let_e
        | "if" exp "then" exp "else" exp -> if_e
        | "-" exp -> neg_e
        | "not" exp -> not_e
        | exp "+" exp -> plus_e
        | exp _cmp exp -> cmp_e
        | "vectorRef" "(" exp "," exp ")" -> vector_ref_e
        | "vectorSet" "(" exp "," exp "," exp ")" -> vector_set_e
        | "vector" "(" exp ("," exp)* ")" -> vector_e
        | funcall_e
        | exp "." CNAME -> field_ref_e
        | "(" exp ")"

    !_cmp: "<"|">"|"=="|">="|"<="|"&&"|"||"

    funcall_e.-500: exp "(" (exp ("," exp)*)? ")"

    def_e: "def" CNAME "(" def_args ")" ":" type "=" "{" exp "}"
    def_args: (typed_name ("," typed_name)*)?

    rec_e: "record" CNAME "{" def_args "}"

    sec_e: "HIGH_SECURITY" "(" (CNAME ("," CNAME)*)? ")"

    typed_name: CNAME ":" type

    ?type: "Integer" -> integer_t
         | "Boolean" -> boolean_t
         | "Vector" "(" type ("," type)* ")" -> vector_t
         | "(" type ("," type)* ")" "->" type -> fun_t
         | CNAME -> rec_t

    prog: sec_e rec_e* def_e* exp

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='prog', parser='lalr')


##################################################
# Pass #0: Parsing Concrete to Abstract Syntax
##################################################

def _parse(s: str) -> RfunProgram:
    def bprog(p) -> RfunProgram:
        assert p.data == 'prog'

        sec_def = p.children[0]
        assert sec_def.data == 'sec_e'

        high_security_variables = [str(x) for x in sec_def.children]
        
        all_defs = [bdef(d) for d in p.children[1:-1]]
        expr = bast(p.children[-1])

        record_defs = []
        fun_defs = []
        for d in all_defs:
            if isinstance(d, RfunDef):
                fun_defs.append(d)
            elif isinstance(d, RfunRecordDef):
                record_defs.append(d)

        return RfunProgram(high_security_variables, record_defs, fun_defs, expr)

    def btype(t):
        if t.data == 'integer_t':
            return IntT()
        elif t.data == 'boolean_t':
            return BoolT()
        elif t.data == 'vector_t':
            arg_types = [btype(ta) for ta in t.children]
            
            return VectorT(arg_types)
        elif t.data == 'fun_t':
            arg_types = [btype(ta) for ta in t.children[:-1]]
            output_type = btype(t.children[-1])
            
            return FunT(arg_types, output_type)
        elif t.data == 'rec_t':
            assert len(t.children) == 1
            rec_name = str(t.children[0])
            return RecordT(rec_name)
        else:
            raise Exception('unknown type:', t)

    def btyped_name(tn):
        assert tn.data == 'typed_name'

        name, t = tn.children
        return (str(name), btype(t))
    
    def bdef(d):
        if d.data == 'def_e':

            name, args, output_type, body = d.children
            assert args.data == 'def_args'
        
            return RfunDef(str(name),
                           [btyped_name(a) for a in args.children],
                           btype(output_type),
                           bast(body))
        elif d.data == 'rec_e':
            name, fields = d.children
            assert fields.data == 'def_args'

            return RfunRecordDef(name, [btyped_name(a) for a in fields.children])

        else:
            raise Exception('unknown def or record:', d)

    def bast(e) -> RfunExp:
        if e.data == 'int_e':
            return Int(int(e.children[0]))
        if e.data == 'var_e':
            return Var(str(e.children[0]))
        elif e.data == 'plus_e':
            e1, e2 = e.children
            return Prim('+', [bast(e1), bast(e2)])
        elif e.data == 'neg_e':
            e1 = e.children[0]
            return Prim('neg', [bast(e1)])
        elif e.data == 'let_e':
            x, e1, body = e.children
            return Let(str(x), bast(e1), bast(body))
        elif e.data == 'if_e':
            e1, e2, e3 = e.children
            return If(bast(e1), bast(e2), bast(e3))
        elif e.data == 'cmp_e':
            e1, op, e2 = e.children
            return Prim(str(op), [bast(e1), bast(e2)])
        elif e.data == 'not_e':
            e1 = e.children[0]
            return Prim('not', [bast(e1)])
        elif e.data == 'true_e':
            return Bool(True)
        elif e.data == 'false_e':
            return Bool(False)
        elif e.data == 'vector_ref_e':
            e1, e2 = e.children
            return Prim('vectorRef', [bast(e1), bast(e2)])
        elif e.data == 'vector_set_e':
            e1, e2, e3 = e.children
            return Prim('vectorSet', [bast(e1), bast(e2), bast(e3)])
        elif e.data == 'vector_e':
            new_children = [bast(c) for c in e.children]
            return Prim('vector', new_children)
        elif e.data == 'funcall_e':
            new_children = [bast(c) for c in e.children]
            return Funcall(new_children[0], new_children[1:])
        elif e.data == 'field_ref_e':
            e1, f = e.children
            return FieldRef(bast(e1), str(f))
        else:
            raise Exception('parse', e)

    parsed = _rfun_parser.parse(s)
    program = bprog(parsed)
    return program

def parse_rfun(s: str) -> RfunProgram:
    return _parse(s)

