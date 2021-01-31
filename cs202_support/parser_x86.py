from lark import Lark

x86_parser = Lark(r"""
    ?instr: "movq" arg "," arg -> movq
          | "addq" arg "," arg -> addq
          | "subq" arg "," arg -> subq
          | "jmp" CNAME -> jmp
          | "callq" CNAME -> callq
          | "pushq" arg -> pushq
          | "popq" arg -> popq
          | "retq" -> retq

    block: CNAME ":" (instr)*

    ?arg: "$" atom -> int_a
        | "%" reg -> reg_a
        | "#" CNAME -> var_a
        | "(" "%" reg ")" -> direct_mem_a
        | atom "(" "%" reg ")" -> mem_a

    ?atom: NUMBER -> int_a
         | "-" atom  -> neg_a

    !?reg: "rsp" | "rbp" | "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" 
         | "r8" | "r9" | "r10" | "r11" | "r12" | "r13" | "r14" | "r15"

    prog: ".globl main" block*

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='prog')

x86_parser_instrs = Lark(r"""
    ?instr: "movq" arg "," arg -> movq
          | "addq" arg "," arg -> addq
          | "subq" arg "," arg -> subq
          | "jmp" CNAME -> jmp
          | "callq" CNAME -> callq
          | "pushq" arg -> pushq
          | "popq" arg -> popq
          | "retq" -> retq

    instrs: instr*

    ?arg: "$" atom -> int_a
        | "%" reg -> reg_a
        | "#" CNAME -> var_a
        | "(" "%" reg ")" -> direct_mem_a
        | atom "(" "%" reg ")" -> mem_a

    ?atom: NUMBER -> int_a
         | "-" atom  -> neg_a

    !?reg: "rsp" | "rbp" | "rax" | "rbx" | "rcx" | "rdx" | "rsi" | "rdi" 
         | "r8" | "r9" | "r10" | "r11" | "r12" | "r13" | "r14" | "r15"

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='instrs')

