from collections import OrderedDict, defaultdict
from typing import List, Set, Dict, Tuple, DefaultDict
import itertools
from rif_parser import *
import sys

from cs202_support.base_ast import AST, print_ast

import cs202_support.x86exp as x86
import cif
import constants

gensym_num = 0


def gensym(x):
    global gensym_num
    gensym_num = gensym_num + 1
    return f'{x}_{gensym_num}'

##################################################
# typecheck
##################################################

@dataclass
class RifType:
    pass

@dataclass
class IntT(RifType):
    pass

@dataclass
class BoolT(RifType):
    pass

TEnv = Dict[str, RifType]

def typecheck(e: RifExp) -> RifExp:
    """
    Typechecks the input program; throws an error if the program is not well-typed.
    :param e: The Rif program to typecheck
    :return: The program, if it is well-typed
    """

    pass

##################################################
# shrink
##################################################

def shrink(e: RifExp) -> RifExp:
    """
    Eliminates some operators from Rif
    :param e: The Rif program to shrink
    :return: A shrunken Rif program
    """

    pass

##################################################
# uniquify
##################################################

def uniquify(e: RifExp) -> RifExp:
    """
    Makes the program's variable names unique
    :param e: The Rif program to uniquify
    :return: An Rif program with unique names
    """

    pass

##################################################
# remove-complex-opera*
##################################################

def mk_let(bindings: Dict[str, RifExp], body: RifExp):
    """
    Builds a Let expression from a list of bindings and a body.
    :param bindings: A list of bindings from variables (str) to their expressions (RifExp)
    :param body: The body of the innermost Let expression
    :return: A Let expression implementing the bindings in "bindings"
    """
    result = body
    for var, rhs in reversed(list(bindings.items())):
        result = Let(var, rhs, result)

    return result


def rco(e: RifExp) -> RifExp:
    """
    Removes complex operands. After this pass, the program will be in A-Normal Form (the arguments to Prim
    operations will be atomic).
    :param e: An Rif expression
    :return: An Rif expression in A-Normal Form
    """

    pass


##################################################
# explicate-control
##################################################

def explicate_control(e: RifExp) -> cif.Program:
    """
    Transforms an Rif Expression into a Cif program.
    :param e: An Rif Expression
    :return: A Cif Program
    """

    pass


##################################################
# select-instructions
##################################################

def select_instructions(p: cif.Program) -> x86.Program:
    """
    Transforms a Cif program into a pseudo-x86 assembly program.
    :param p: a Cif program
    :return: a pseudo-x86 program
    """

    pass


##################################################
# uncover-live
##################################################

def uncover_live(program: x86.Program) -> Tuple[x86.Program, Dict[str, List[Set[x86.Var]]]]:
    """
    Performs liveness analysis. Returns the input program, plus live-after sets for its blocks.
    :param program: a pseudo-x86 assembly program
    :return: A tuple. The first element is the same as the input program. The second element is a dict of
    live-after sets. This dict maps each label in the program to a list of live-after sets for that label.
    The live-after sets are in the same order as the label's instructions.
    """

    pass


##################################################
# build-interference
##################################################

class InterferenceGraph:
    """
    A class to represent an interference graph: an undirected graph where nodes are x86.Arg objects and an edge
    between two nodes indicates that the two nodes cannot share the same locations.
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
        pairs = set()
        for k in self.graph.keys():
            new_pairs = set((k, v) for v in self.graph[k])
            pairs = pairs.union(new_pairs)

        for a, b in list(pairs):
            if (b, a) in pairs:
                pairs.remove((a, b))

        strings = [print_ast(a) + ' -- ' + print_ast(b) for a, b in pairs]
        return 'InterferenceGraph{\n ' + ',\n '.join(strings) + '\n}'



def build_interference(inputs: Tuple[x86.Program, Dict[str, List[Set[x86.Var]]]]) -> \
        Tuple[x86.Program, InterferenceGraph]:
    """
    Build the interference graph.
    :param inputs: A Tuple. The first element is a pseudo-x86 program. The second element is the dict of live-after
    sets produced by the uncover-live pass.
    :return: A Tuple. The first element is the same as the input program. The second is a completed interference graph.
    """

    pass


##################################################
# allocate-registers
##################################################

Color = int
Coloring = Dict[x86.Var, Color]
Saturation = Set[Color]


def allocate_registers(inputs: Tuple[x86.Program, InterferenceGraph]) -> \
        Tuple[x86.Program, int]:
    """
    Assigns homes to variables in the input program. Allocates registers and stack locations as needed, based on
    a graph-coloring register allocation algorithm.
    :param inputs: A Tuple. The first element is the pseudo-x86 program. The second element is the completed
    interference graph.
    :return: A Tuple. The first element is an x86 program (with no variable references). The second element is
    the number of bytes needed in stack locations.
    """

    pass


##################################################
# patch-instructions
##################################################

def patch_instructions(inputs: Tuple[x86.Program, int]) -> Tuple[x86.Program, int]:
    """
    Patches instructions with two memory location inputs, using %rax as a temporary location.
    :param inputs: A Tuple. The first element is an x86 program. The second element is the stack space in bytes.
    :return: A Tuple. The first element is the patched x86 program. The second element is the stack space in bytes.
    """

    pass


##################################################
# print-x86
##################################################

def print_x86(inputs: Tuple[x86.Program, int]) -> str:
    """
    Prints an x86 program to a string.
    :param inputs: A Tuple. The first element is the x86 program. The second element is the stack space in bytes.
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
    'remove complex opera*': rco,
    'explicate control': explicate_control,
    'select instructions': select_instructions,
    'uncover live': uncover_live,
    'build interference': build_interference,
    'allocate registers': allocate_registers,
    'patch instructions': patch_instructions,
    'print x86': print_x86
}


def run_compiler(s, logging=False):
    """
    Run the compiler on an input program.
    :param s: An Rif program, as a string.
    :param logging: Whether or not to print out debugging information.
    :return: An x86 program, as a string
    """
    current_program = parse_rif(s)

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

            program = f.read()
            x86_program = run_compiler(program, logging=True)

            with open(file_name + '.s', 'w') as output_file:
                output_file.write(x86_program)
