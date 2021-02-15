# reg ::= rsp | rbp | rax | rbx | rcx | rdx | rsi | rdi |
# r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
# arg ::= $int | %reg | int(%reg)
# instr ::= addq arg,arg | subq arg,arg | negq arg | movq arg,arg |
# callq label | pushq arg | popq arg | retq | jmp label

from dataclasses import dataclass
from typing import List, Set, Dict, Tuple, Any
from .base_ast import AST

# arg
@dataclass
class Arg(AST):
    pass

@dataclass
class Int(Arg):
    val: int

@dataclass
class Reg(Arg):
    val: str

@dataclass
class Var(Arg):
    var: str

@dataclass
class Deref(Arg):
    offset: int
    val: str

# instr
@dataclass
class Instr(AST):
    pass

@dataclass
class Addq(Instr):
    e1: Arg
    e2: Arg

@dataclass
class Subq(Instr):
    e1: Arg
    e2: Arg

@dataclass
class Negq(Instr):
    e1: Arg

@dataclass
class Movq(Instr):
    e1: Arg
    e2: Arg

@dataclass
class Callq(Instr):
    label: str

@dataclass
class Jmp(Instr):
    label: str

@dataclass
class Pushq(Instr):
    e1: Arg

@dataclass
class Popq(Instr):
    e1: Arg

@dataclass
class Retq(Instr):
    pass

@dataclass
class Program(AST):
    blocks: Dict[str, List[Instr]]
