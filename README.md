# CS202 Assignment Code and Support

We will be implementing our compilers in Python. This repository
contains the support code and assignment scaffolding you will need.

## Installing Conda and Jupyter Notebook

We will be writing Python code for assignments in regular files, and
completing exercises in Jupyter notebooks. To set up a Python
installation that supports Jupyter notebooks. I recommend using Conda.
Please [click
here](https://github.com/jnear/cs211-data-privacy/blob/master/jupyter.md)
for information on installing Conda and setting up Jupyter notebooks.

## Installing Python with Homebrew

Homebrew is an open source package manager for MacOS. To install homebrew run their installation command on your terminal. You can find 
the command on their [website](https://brew.sh/)

Once you have homebrew to installed, you can install any version of python using their CLI. In this case I am installing version 3.10 other 
versions are also supported, you can find more information in the [python formulae](https://docs.brew.sh/Homebrew-and-Python)

```bash
brew install python@3.10
```

## Creating the CS202 Environment

Python version 3.10 is required for CS202 (to support pattern
matching). I recommend creating a fresh Conda environment for CS202,
to ensure you have the right version of Python and the appropriate
libraries. You can create the environment as follows:

1. Open a terminal with Conda support. On Windows, launch "Anaconda
   Prompt"; on Linux or MacOS, launch a regular terminal.
2. To create the environment, type: `conda create -n cs202 python=3.10`
3. To activate the environment, type `conda activate cs202`. The
   prompt's prefix should change from `(base)` to `(cs202)`.

## Installing the Support Code

You can install the support code for CS202 using `pip`:

1. Open a terminal with Conda support (as above)
2. Activate the Conda env: `conda activate cs202`
3. Install the code: `pip install git+https://github.com/jnear/cs202-assignments.git`

Alternatively, if you have cloned the repository on your computer:

1. Open a terminal with Conda support (as above)
2. Activate the Conda env: `conda activate cs202`
3. Navigate to the `cs202-assignments` directory (e.g. using the `cd` command)
4. Install the code: `pip install .`

## Installing and Using PyCharm

I recommend using the PyCharm "community edition" to edit your
assignment code code. PyCharm supports Python 3's static type hints,
and can help you avoid difficult-to-debug errors when implementing
your compiler. You can find information on downloading and installing
it [at this link](https://www.jetbrains.com/pycharm/download/).

You can set up PyCharm to work with your Conda environment as follows:

1. Click the "open" button in PyCharm to open a project
2. Select the directory where you checked out this repo (if you're
   asked to create a virtual env, click "cancel")
3. Open Settings (File -> Settings, or PyCharm -> Preferences on MacOS)
4. Open the "Project: cs202-assignments" -> "Python Interpreter"
   section of the settings
5. Click the settings gear icon next to the "Python Interpreter"
   drop-down box, and click "Add"
6. On the left side, select "Conda Environment"
7. Click the "Existing Environment" option
8. In the "Interpreter" drop-down, pick the cs202 environment that you
   created earlier
9. Click "OK" twice
   
If the cs202 environment doesn't appear (step 8), click the "open"
button next to the drop-down and select the Python executable in the
cs202 environment. You can find its location by typing `conda env
list` at a Conda-enabled terminal. This command lists all environments
and their location. The Python executable for an environment can be
found at `bin/python` in that environment's directory.

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

You can run (or debug) the compiler in PyCharm by creating a run
configuration with `compiler.py` as the target and the filename of a
test as the command-line argument. You can run the tests by creating a
configuration with `run_tests.py` as the target.

## Online Compiler Diff Checker
This script will display the diffs of your compiler compared to the online compiler. By default it checks the prelude and conclusion pass but you can
specify a pass the check. 

You may need to pip install a few packages if any of the imports are unrecognized when running the script. Use `pip install [name of package]`.

Run `python verify.py --help` for specific usage

Verify all tests in the `a2/tests` dir with the `a2` compiler
```
python verify.py a2/tests/ a2`
```

Verify the assign homes pass of the `a2/tests/test1.py` file with the `a2` compiler
```
python verify.py a2/tests/test1.py a2 -v ah
```

Verify all tests in the `a2/tests` dir with the `a2` online compiler and your local `a1` compiler
```
python verify.py a2/tests/ a2 -l a1
```

Use the `-d` flag to show the strings that were extracted from both compilers. If the local compiler string is not an AST, there is probably an issue
with your compiler. Run your compiler on a test case like `python compiler.py tests/test1.py` to determine what the error is.

## Assignment Submission

This repository contains the skeleton of each assignment's
solution. The only file you will need to change is `compiler.py`. When
you submit your assignment solution on Blackboard, you should upload
*only* the `compiler.py` file. Please do not change any other files; I
won't have access to changes you make to other files when grading your
assignments.

## Useful Tips

To install the CS202 support code *inside* a Jupyter notebook, put the
following code in a cell and run it:

```
!pip install git+https://github.com/jnear/cs202-assignments
```

If you try to run `jupyter notebook` in your Conda environment, and
get a command not found error, try:

```
pip install jupyter
```

If you try the above, and get a command not found error for `pip`,
try:

```
conda install pip
```

and then try installing Jupyter again.

## FAQs on Completing Assignments

#### What is the point of this pass? What is it trying to do?
- Look at the relevant section in the textbook; the first sentence or paragraph describes the goal of the pass and often why it's needed
- Look at the relevant section in the most recent exercise; the first note or first few questions often describes the point of the pass and its high level idea

#### How do I get started on a pass?
- Build the structure of the pass *without thinking*
  - Look at the grammar for the input to the pass
    - Create a nested function for each production rule (the left-hand side of ::=)
    - Create a match statement inside each nested function
    - Create a match case for each case of that production rule in the grammar (the right-hand side of ::=)
  - Most exercise sections describing a pass will include a final question of the form "describe the pass in the compiler" whose answer gives an overview for the implementation strategy of the pass
    - Copy and paste these instructions into your code as comments
    - Follow the comments as you implement the function

#### How do I fill in the implementation of the pass?
- Fill in the structure by thinking
  - Consult the textbook
  - Consult the exercises and your notes
  - Try an example in the online compiler
  - Try an example on paper
  - Re-do the relevant exercise questions
