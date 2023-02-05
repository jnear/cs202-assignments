from collections import defaultdict

from typing import List, Set, Dict, Tuple, DefaultDict
import sys
import itertools
import traceback

from cs202_support.python import *
import cs202_support.x86 as x86
import constants
from interference_graph import InterferenceGraph

gensym_num = 0
global_logging = False

def log(label, value):
    if global_logging:
        print()
        print(f'--------------------------------------------------')
        print(f'Logging: {label}')
        print(value)
        print(f'--------------------------------------------------')

def log_ast(label, value):
    log(label, print_ast(value))

def gensym(x):
    """
    Constructs a new variable name guaranteed to be unique.
    :param x: A "base" variable name (e.g. "x")
    :return: A unique variable name (e.g. "x_1")
    """

    global gensym_num
    gensym_num = gensym_num + 1
    return f'{x}_{gensym_num}'


##################################################
# remove-complex-opera*
##################################################

def rco(prog: Program) -> Program:
    """
    Removes complex operands. After this pass, the arguments to operators (unary and binary
    operators, and function calls like "print") will be atomic.
    :param prog: An Lvar program
    :return: An Lvar program with atomic operator arguments.
    """

    pass


##################################################
# select-instructions
##################################################

# Output language: pseudo-x86
# Arg ::= Immediate(n) | Var(x) | Reg(str)
# instr_name ::= "movq" | "addq"
# Instr ::= NamedInstr(instr_name, [Arg]) | Callq(str) | Retq()
# X86 ::= X86Program(Dict[str, [Instr]])

def select_instructions(prog: Program) -> x86.X86Program:
    """
    Transforms a Lvar program into a pseudo-x86 assembly program.
    :param prog: a Lvar program
    :return: a pseudo-x86 program
    """

    pass


##################################################
# allocate-registers
##################################################

Color = int
Coloring = Dict[x86.Var, Color]
Saturation = Set[Color]

# Input and Output language: pseudo-x86
# Arg ::= Immediate(n) | Var(x) | Reg(str)
# instr_name ::= "movq" | "addq"
# Instr ::= NamedInstr(instr_name, [Arg]) | Callq(str) | Retq()
# X86 ::= X86Program(Dict[str, [Instr]])

def allocate_registers(program: x86.X86Program) -> x86.X86Program:
    """
    Assigns homes to variables in the input program. Allocates registers and
    stack locations as needed, based on a graph-coloring register allocation
    algorithm.
    :param program: A pseudo-x86 program.
    :return: An x86 program, annotated with the number of bytes needed in stack
    locations.
    """

    # --------------------------------------------------
    # utilities
    # --------------------------------------------------

    # --------------------------------------------------
    # liveness analysis
    # --------------------------------------------------
    def ul_instr(i: x86.Instr, live_after: Set[x86.Var]) -> Set[x86.Var]:
        pass

    def ul_block(instrs: List[x86.Instr]) -> List[Set[x86.Var]]:
        pass

    # --------------------------------------------------
    # interference graph
    # --------------------------------------------------
    def bi_instr(e: x86.Instr, live_after: Set[x86.Var], graph: InterferenceGraph):
        pass

    def bi_block(instrs: List[x86.Instr], live_afters: List[Set[x86.Var]], graph: InterferenceGraph):
        pass

    # --------------------------------------------------
    # graph coloring
    # --------------------------------------------------
    def color_graph(local_vars: Set[x86.Var], interference_graph: InterferenceGraph) -> Coloring:
        pass

    # --------------------------------------------------
    # assigning homes
    # --------------------------------------------------

    # --------------------------------------------------
    # main body of the pass
    # --------------------------------------------------

    # Step 1: Perform liveness analysis
    blocks = program.blocks
    # TODO: run the liveness analysis
    live_after_sets = None
    log_ast('live-after sets', live_after_sets)

    # Step 2: Build the interference graph
    interference_graph = InterferenceGraph()

    # TODO: build the interference graph

    log_ast('interference graph', interference_graph)

    # Step 3: Color the graph
    coloring = None

    # TODO: color the interference graph

    log('coloring', coloring)

    # Defines the set of registers to use
    available_registers = constants.caller_saved_registers + constants.callee_saved_registers

    # Step 4: map variables to homes
    color_map = {}
    stack_locations_used = 0

    # Step 4.1: Map colors to locations (the "color map")
    # TODO: step 4.1

    # Step 4.2: Compose the "coloring" with the "color map" to get "homes"
    # TODO: step 4.2
    log('homes', homes)

    # Step 5: replace variables with their homes
    # TODO: step 5
    pass


##################################################
# patch-instructions
##################################################

def patch_instructions(program: x86.X86Program) -> x86.X86Program:
    """
    Patches instructions with two memory location inputs, using %rax as a temporary location.
    :param program: An x86 program.
    :return: A patched x86 program.
    """

    pass


##################################################
# prelude-and-conclusion
##################################################

def prelude_and_conclusion(program: x86.X86Program) -> x86.X86Program:
    """
    Adds the prelude and conclusion for the program.
    :param program: An x86 program.
    :return: An x86 program, with prelude and conclusion.
    """

    pass


##################################################
# Compiler definition
##################################################

compiler_passes = {
    'remove complex opera*': rco,
    'select instructions': select_instructions,
    'allocate registers': allocate_registers,
    'patch instructions': patch_instructions,
    'prelude & conclusion': prelude_and_conclusion,
    'print x86': x86.print_x86
}


def run_compiler(s, logging=False):
    global global_logging
    global_logging = logging

    def print_prog(current_program):
        print('Concrete syntax:')
        if isinstance(current_program, x86.X86Program):
            print(x86.print_x86(current_program))
        elif isinstance(current_program, Program):
            print(print_program(current_program))

        print()
        print('Abstract syntax:')
        print(print_ast(current_program))

    current_program = parse(s)

    if logging == True:
        print()
        print('==================================================')
        print(' Input program')
        print('==================================================')
        print()
        print_prog(current_program)

    for pass_name, pass_fn in compiler_passes.items():
        current_program = pass_fn(current_program)

        if logging == True:
            print()
            print('==================================================')
            print(f' Output of pass: {pass_name}')
            print('==================================================')
            print()
            print_prog(current_program)

    return current_program


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: python compiler.py <source filename>')
    else:
        file_name = sys.argv[1]
        with open(file_name) as f:
            print(f'Compiling program {file_name}...')

            try:
                program = f.read()
                x86_program = run_compiler(program, logging=True)

                with open(file_name + '.s', 'w') as output_file:
                    output_file.write(x86_program)

            except:
                print('Error during compilation! **************************************************')
                traceback.print_exception(*sys.exc_info())
