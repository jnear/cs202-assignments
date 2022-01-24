from ast import *
from typing import List, Set, Dict, Tuple
import sys

from cs202_support.base_ast import print_ast

import cs202_support.x86exp as x86


##################################################
# select-instructions
##################################################

def select_instructions(program: Module) -> x86.Program:
    """
    Transforms a Lmin program into a pseudo-x86 assembly program.
    :param program: a Lmin program
    :return: a pseudo-x86 program
    """

    # YOUR CODE HERE
    pass


##################################################
# print-x86
##################################################

def print_x86(program: x86.Program) -> str:
    """
    Prints an x86 program to a string.
    :param program: An x86 program.
    :return: A string, ready for gcc.
    """

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
    current_program = parse(s)

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
                input_program = f.read()
                x86_program = run_compiler(input_program, logging=True)

                with open(file_name + '.s', 'w') as output_file:
                    output_file.write(x86_program)

            except Exception as e:
                raise Exception('Error during compilation:', e)
