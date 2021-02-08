# CS202 Assignment Code and Support

We will be implementing our compilers in Python. This repository
contains the support code and assignment scaffolding you will need.

## Setting up Jupyter Notebook

We will be writing Python code for assignments in regular files, and
completing exercises in Jupyter notebooks. To set up a Python
installation that supports Jupyter notebooks. I recommend using
Anaconda. Please [click
here](https://github.com/jnear/cs211-data-privacy/blob/master/jupyter.md)
for information on setting up Jupyter notebooks.

## Installing PyCharm

I recommend using the PyCharm "community edition" to edit your
assignment code code. PyCharm supports Python 3's static type hints,
and can help you avoid difficult-to-debug errors when implementing
your compiler. You can find information on downloading and installing
it [at this link](https://www.jetbrains.com/pycharm/download/).

## Installing the Support Code

You can install the support code for CS202 using `pip`. Clone this
repository, navigate to the cloned directory, and type:

```
pip install .
```

## Building the Runtime

We will test our x86 assembly code using both an emulator and direct
execution on your hardware. To assemble your code into a binary,
you'll need a runtime system. The runtime is implemented in C, in the
file `runtime.c`. You can compile the runtime into an object file
(`runtime.o`) as follows:

```
gcc -c -g -std=c99 runtime.c
```

This will produce a file named `runtime.o`. The -g flag is to tell the
compiler to produce debug information that you may need to use the gdb
(or lldb) debugger.

Next, suppose your compiler has produced the x86 assembly program file
`foo.s` (the `.s` filename extension is the standard one for assembly
programs). To produce an executable program, you can then run:

```
gcc -g runtime.o foo.s
```

which will produce the executable program named `a.out` by linking
your program with the runtime.

## Running the Compiler & Tests

To compile a program into an assembly file, navigate to the directory
containing the compiler implementation and run `compile.py`. For example:

```
cd a1/
python compiler.py tests/test1.r0
```

will produce the file `tests/test1.r0.s`.

To run your compiled program, first use GCC to assemble it into a
binary, then run the resulting binary (which will be called `a.out`):

```
gcc -g ../runtime.o tests/test1.r0.s
./a.out
```

The process above runs a single program and allows you to view its
output.

To run all of the tests, navigate to the directory containing the
compiler implementation and run `run_tests.py`. For example:

```
cd a1/
python run_tests.py
```

This process allows you to quickly verify that all of the test cases
pass, but does not print out the output of each compiler pass.

## Assignment Submission

This repository contains the skeleton of each assignment's
solution. The only file you will need to change is `compiler.py`. When
you submit your assignment solution on Blackboard, you should upload
*only* the `compiler.py` file. Please do not change any other files; I
won't have access to changes you make to other files when grading your
assignments.
