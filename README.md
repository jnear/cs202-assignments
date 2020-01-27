# CS202 Assignment Code

## Setting up Haskell and Stack

We will implement our compilers in Haskell, and build them using
Stack, a build system for Haskell. 

Stack is available for Windows, MacOS, Linux, BSD, and more. To
install Stack, following the instructions
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/). If
you run into problems installing Stack, please post to Piazza or email
the instructor.

I recommend using Atom, emacs, or Vim to edit Haskell code. You may
find [Hoogle](https://hoogle.haskell.org/) useful for looking up
Haskell library functions.

## Building the Runtime

The second step is to build the runtime system that our compiled
programs will use. The runtime is implemented in C, in the file
`runtime.c`. You can compile the runtime into an object file
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
containing the compiler implementation and run `Main.hs`. For example:

```
cd a1/
stack runghc Main.hs tests/test1.r0
```

will produce the file `tests/test1.s`.

To run your compiled program, first use GCC to assemble it into a
binary, then run the resulting binary (which will be called `a.out`):

```
gcc -g ../runtime.o tests/test1.s
./a.out
```

The process above runs a single program and allows you to view its
output.

To run all of the tests, navigate to the directory containing the
compiler implementation and run `RunTests.hs`. For example:

```
cd a1/
stack runghc RunTests.hs
```

This process allows you to quickly verify that all of the test cases
pass, but does not print out the output of each compiler pass.

## Assignment Submission

This repository contains the skeleton of each assignment's
solution. The only file you will need to change is `Compiler.hs`. When
you submit your assignment solution on Blackboard, you should upload
*only* the `Compiler.hs` file. Please do not change any other files; I
won't have access to changes you make to other files when grading your
assignments.
