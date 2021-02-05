import os
from compiler import run_compiler
from r0_parser import parse_rvar
from interpreter import eval_rvar
from cs202_support import eval_x86


for file_name in sorted(os.listdir('tests')):
    if file_name.endswith('.r0'):
        with open('tests/' + file_name) as f:
            print(f'Testing program {file_name}...')

            try:
                program = f.read()
                ast = parse_rvar(program)
                interpreter_result = eval_rvar(ast)
            
                x86_program = run_compiler(program, logging=False)
                emu = eval_x86.X86Emulator(logging=False)
                x86_output = emu.eval_program(x86_program)

                if len(x86_output) == 1 and x86_output[0] == interpreter_result:
                    print('Test passed')
                else:
                    print('Test failed!')
                    print('Interpreter result:', interpreter_result)
                    print('Compiled x86 result:', x86_output)

                print()

            except Exception as e:
                print('Test FAILED! Error:', e)
                print()
