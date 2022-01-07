from dataclasses import dataclass
from collections import OrderedDict
from typing import List, Set, Dict, Tuple
from r0_parser import *
import sys

from cs202_support.base_ast import AST, print_ast

import cs202_support.x86exp as x86

gensym_num = 0
def gensym(x):
    global gensym_num
    gensym_num = gensym_num + 1
    return f'{x}_{gensym_num}'

##################################################
# Pass #1: select-instructions
##################################################

def select_instructions(e: R0Exp) -> x86.Program:
    # YOUR CODE HERE
    pass
    
##################################################
# Pass #2: print-x86
##################################################

def print_x86(program: x86.Program) -> str:
    # YOUR CODE HERE
    pass

##################################################
# Compiler definition
##################################################

compiler_passes = {
    'select instructions': select_instructions,
    'print x86': print_x86
}
    

def run_compiler(s, logging=False):
    current_program = parse_rvar(s)

    if logging == True:
        print()
        print('==================================================')
        print(' Input program')
        print('==================================================')
        print()
        print(print_ast(current_program))


    for pass_name, pass_fn in compiler_passes.items():
        current_program = pass_fn(current_program)

        if logging == True:
            print()
            print('==================================================')
            print(f' Output of pass: {pass_name}')
            print('==================================================')
            print()
            print(print_ast(current_program))

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

            except Exception as e:
                print('Error during compilation:', e)
