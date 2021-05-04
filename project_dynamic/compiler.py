from collections import OrderedDict, defaultdict
from typing import List, Set, Dict, Tuple, DefaultDict, Union
import itertools
import textwrap

from rfun_parser import *
from typed_rfun import *

import sys

from cs202_support.base_ast import AST, print_ast

import cs202_support.x86exp as x86
import cfun
import constants

gensym_num = 0


def gensym(x):
    global gensym_num
    gensym_num = gensym_num + 1
    return f'{x}_{gensym_num}'

def unzip2(ls):
    """
    Unzip a list of 2-tuples.
    :param ls: A list of 2-tuples.
    :return: A single 2-tuple. The first element of the tuple is a list of the 
    first elements of the pairs in ls. The second element of the tuple is a list
    of the second elements of the pairs in ls.
    """

    if ls == []:
        return [], []
    else:
        xs, ys = zip(*ls)
        return list(xs), list(ys)

##################################################
# typecheck
##################################################

TEnv = Dict[str, RfunType]

def typecheck(p: RfunProgram) -> RfunProgramT:
    """
    Typechecks the input program; throws an error if the program is not well-typed.
    :param e: The Rfun program to typecheck
    :return: The program, if it is well-typed
    """

    pass

##################################################
# shrink
##################################################

def shrink(p: RfunProgramT) -> RfunProgramT:
    """
    Eliminates some operators from Rfun
    :param e: The Rfun program to shrink
    :return: A shrunken Rfun program
    """

    pass
    

##################################################
# uniquify
##################################################

def uniquify(p: RfunProgramT) -> RfunProgramT:
    """
    Makes the program's variable names unique
    :param e: The Rfun program to uniquify
    :return: An Rfun program with unique names
    """

    pass


##################################################
# reveal_functions
##################################################

def reveal_functions(p: RfunProgramT) -> RfunProgramT:
    """
    Transform references to top-level functions from variable references to
    function references.
    :param e: An Rfun program
    :return: An Rfun program in which all references to top-level functions
    are in the form of FunRef objects.
    """

    pass

##################################################
# limit-functions
##################################################


def limit_functions(p: RfunProgramT) -> RfunProgramT:
    """
    Limit functions to have at most 6 arguments.
    :param e: An Rfun program to reveal_functions
    :return: An Rfun program, in which each function has at most 6 arguments
    """

    pass


##################################################
# expose-alloc
##################################################

def mk_let(bindings: Dict[str, RfunExpT], body: RfunExpT):
    """
    Builds a Let expression from a list of bindings and a body.
    :param bindings: A list of bindings from variables (str) to their 
    expressions (RfunExp)
    :param body: The body of the innermost Let expression
    :return: A Let expression implementing the bindings in "bindings"
    """
    result = body
    for var, rhs in reversed(list(bindings.items())):
        result = LetTE(var, rhs, result)

    return result


def expose_alloc(p: RfunProgramT) -> RfunProgramT:
    """
    Transforms 'vector' forms into explicit memory allocations, with conditional
    calls to the garbage collector.
    :param e: A typed Rfun expression
    :return: A typed Rfun expression, without 'vector' forms
    """

    pass


##################################################
# remove-complex-opera*
##################################################

def rco(p: RfunProgramT) -> RfunProgramT:
    """
    Removes complex operands. After this pass, the program will be in A-Normal
    Form (the arguments to Prim operations will be atomic).
    :param e: An Rfun expression
    :return: An Rfun expression in A-Normal Form
    """

    pass


##################################################
# explicate-control
##################################################

def explicate_control(p: RfunProgramT) -> cfun.Program:
    """
    Transforms an Rfun Program into a Cfun program.
    :param e: An Rfun Program
    :return: A Cfun Program
    """

    pass


##################################################
# select-instructions
##################################################

def select_instructions(p: cfun.Program) -> Dict[str, x86.Program]:
    """
    Transforms a Cfun program into a pseudo-x86 assembly program.
    :param p: a Cfun program
    :return: a set of pseudo-x86 definitions, as a dictionary mapping function
    names to pseudo-x86 programs.
    """

    pass


##################################################
# uncover-live
##################################################

def uncover_live(program: Dict[str, x86.Program]) -> \
    Tuple[Dict[str, x86.Program],
          Dict[str, List[Set[x86.Var]]]]:
    """
    Performs liveness analysis. Returns the input program, plus live-after sets
    for its blocks.
    :param program: pseudo-x86 assembly program definitions
    :return: A tuple. The first element is the same as the input program. The
    second element is a dict of live-after sets. This dict maps each label in
    the program to a list of live-after sets for that label. The live-after 
    sets are in the same order as the label's instructions.
    """

    pass


##################################################
# build-interference
##################################################

class InterferenceGraph:
    """
    A class to represent an interference graph: an undirected graph where nodes 
    are x86.Arg objects and an edge between two nodes indicates that the two
    nodes cannot share the same locations.
    """
    graph: DefaultDict[x86.Arg, Set[x86.Arg]]

    def __init__(self):
        self.graph = defaultdict(lambda: set())

    def add_edge(self, a: x86.Arg, b: x86.Arg):
        if a != b:
            self.graph[a].add(b)
            self.graph[b].add(a)

    def neighbors(self, a: x86.Arg) -> Set[x86.Arg]:
        if a in self.graph:
            return self.graph[a]
        else:
            return set()

    def __str__(self):
        strings = []
        for k, v in dict(self.graph).items():
            if isinstance(k, (x86.Var, x86.VecVar)):
                t = ', '.join([print_ast(i) for i in v])
                tt = '\n      '.join(textwrap.wrap(t))
                strings.append(f'{print_ast(k)}: {tt}')
        lines = '\n  '.join(strings)
        return f'InterferenceGraph (\n  {lines}\n )\n'



def build_interference(inputs: Tuple[Dict[str, x86.Program],
                                     Dict[str, List[Set[x86.Var]]]]) -> \
        Tuple[Dict[str, x86.Program],
              Dict[str, InterferenceGraph]]:
    """
    Build the interference graph.
    :param inputs: A Tuple. The first element is a pseudo-x86 program. The 
    second element is the dict of live-after sets produced by the uncover-live 
    pass.
    :return: A Tuple. The first element is the same as the input program. 
    The second is a dict mapping each function name to its completed 
    interference graph.
    """

    pass


##################################################
# allocate-registers
##################################################


Color = int
Coloring = Dict[x86.Var, Color]
Saturation = Set[Color]

def allocate_registers(inputs: Tuple[Dict[str, x86.Program],
                                     Dict[str, InterferenceGraph]]) -> \
    Dict[str, Tuple[x86.Program, int, int]]:
    """
    Assigns homes to variables in the input program. Allocates registers and 
    stack locations as needed, based on a graph-coloring register allocation 
    algorithm.
    :param inputs: A Tuple. The first element is the pseudo-x86 program. The
    second element is a dict mapping function names to interference graphs.

    :return: A dict mapping each function name to a Tuple. The first element
    of each tuple is an x86 program (with no variable references). The second
    element is the number of bytes needed in regular stack locations. The third
    element is the number of variables spilled to the root (shadow) stack.
    """

    pass


##################################################
# patch-instructions
##################################################

def patch_instructions(inputs: Dict[str, Tuple[x86.Program, int, int]]) -> \
    Dict[str, Tuple[x86.Program, int, int]]:
    """
    Patches instructions with two memory location inputs, using %rax as a 
    temporary location.
    :param inputs: A dict mapping each function name to a Tuple. The first
    element of each tuple is an x86 program. The second element is the stack
    space in bytes. The third is the number of variables spilled to the root
    stack.
    :return: A Tuple. The first element is the patched x86 program. The second
    and third elements stay the same.
    """

    pass


##################################################
# print-x86
##################################################

def print_x86(inputs: Dict[str, Tuple[x86.Program, int, int]]) -> str:
    """
    Prints an x86 program to a string.
    :param inputs: A dict mapping each function name to a Tuple. The first
    element of the Tuple is an x86 program. The second element is the stack
    space in bytes. The third is the number of variables spilled to the
    root stack.
    :return: A string, ready for gcc.
    """

    pass


##################################################
# Compiler definition
##################################################

compiler_passes = {
    'typecheck': typecheck,
    'shrink': shrink,
    'uniquify': uniquify,
    'reveal functions': reveal_functions,
    'limit functions': limit_functions,
    'expose allocation': expose_alloc,
    'remove complex opera*': rco,
    'explicate control': explicate_control,
    'select instructions': select_instructions,
    'uncover live': uncover_live,
    'build interference': build_interference,
    'allocate registers': allocate_registers,
    'patch instructions': patch_instructions,
    'print x86': print_x86
}


def run_compiler(s: str, logging=False) -> str:
    """
    Run the compiler on an input program.
    :param s: An Rfun program, as a string.
    :param logging: Whether or not to print out debugging information.
    :return: An x86 program, as a string
    """
    current_program = parse_rfun(s)

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

    assert isinstance(current_program, str)
    return current_program


if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('Usage: python compiler.py <source filename>')
    else:
        file_name = sys.argv[1]
        with open(file_name) as f:
            print(f'Compiling program {file_name}...')

            program = f.read()
            x86_program = run_compiler(program, logging=True)

            with open(file_name + '.s', 'w') as output_file:
                output_file.write(x86_program)
