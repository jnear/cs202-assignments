from lark import Lark

x86_parser = Lark(r"""
    ?instr: "movq" arg "," arg -> movq
          | "addq" arg "," arg -> addq
          | "subq" arg "," arg -> subq
          | "cmpq" arg "," arg -> cmpq
          | "xorq" arg "," arg -> xorq
          | "negq" arg -> negq
          | "jmp" CNAME -> jmp
          | "je" CNAME -> je
          | "jl" CNAME -> jl
          | "jle" CNAME -> jle
          | "jg" CNAME -> jg
          | "jge" CNAME -> jge
          | "sete" arg -> sete
          | "setl" arg -> setl
          | "setle" arg -> setle
          | "setg" arg -> setg
          | "setge" arg -> setge
          | "movzbq" arg "," arg -> movzbq
          | "xorq" arg "," arg -> xorq
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
         | "al"

    prog: ".globl main" block*

    %import common.NUMBER
    %import common.CNAME

    %import common.WS
    %ignore WS
    """, start='prog', parser='lalr')

x86_parser_instrs = Lark(r"""
    ?instr: "movq" arg "," arg -> movq
          | "addq" arg "," arg -> addq
          | "subq" arg "," arg -> subq
          | "cmpq" arg "," arg -> cmpq
          | "xorq" arg "," arg -> xorq
          | "negq" arg -> negq
          | "jmp" CNAME -> jmp
          | "je" CNAME -> je
          | "jl" CNAME -> jl
          | "jle" CNAME -> jle
          | "jg" CNAME -> jg
          | "jge" CNAME -> jge
          | "sete" arg -> sete
          | "setl" arg -> setl
          | "setle" arg -> setle
          | "setg" arg -> setg
          | "setge" arg -> setge
          | "movzbq" arg "," arg -> movzbq
          | "xorq" arg "," arg -> xorq
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
    """, start='instrs', parser='lalr')

