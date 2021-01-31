{-# LANGUAGE Strict, ScopedTypeVariables #-}
module Compiler where

import Data.List
import Data.Maybe
import Text.Pretty.Simple (pPrint, pPrintNoColor)

import Gensym
import AST


-- a Type Alias for variables
-- Variables are represented by Strings
type Variable = String

------------------------------------------------------------
-- uniquify
------------------------------------------------------------

-- A Variable Environment maps variables to variables
type VEnv = [(Variable, Variable)]

-- The uniquify pass, for an expression
-- Input:
--   - an R1 expression
--   - a variable environment
-- Output: an R1 expression, with variables renamed to be unique
uniquifyExp :: R1Expr -> VEnv -> R1Expr
uniquifyExp e env = undefined

-- The uniquify pass, for an R1 program
-- Input:  an R1 expression
-- Output: an R1 expression, with variables renamed to be unique
-- This function simply calls `uniquifyExp` with the empty variable environment
uniquify :: R1Expr -> R1Expr
uniquify e = uniquifyExp e []

------------------------------------------------------------
-- remove-complex-opera
------------------------------------------------------------

-- a Binding maps a variable to an expression
type Binding = (Variable, R1Expr)

-- The remove-complex-operand pass on an expression in TAIL POSITION
-- input:  COMPLEX EXPRESSION
-- output: COMPLEX EXPRESSION in A-Normal Form
rcoExp :: R1Expr -> R1Expr
rcoExp e = undefined

-- The remove-complex-operand pass on an expression in ARGUMENT POSITION
-- input:  COMPLEX EXPRESSION
-- output: pair: SIMPLE EXPRESSION and LIST OF BINDINGS
-- 
-- the LIST OF BINDINGS maps variables to SIMPLE EXPRESSIONS
rcoArg :: R1Expr -> (R1Expr, [Binding])
rcoArg e = undefined

-- Make a "LET" expression from a list of bindings and a final "body" expression
mkLet :: [Binding] -> R1Expr -> R1Expr
mkLet [] body = body
mkLet ((x, e) : bs) body = LetE x e (mkLet bs body)

------------------------------------------------------------
-- explicate-control
------------------------------------------------------------

data C0Arg = IntC0 Int
           | VarC0 Variable
  deriving (Eq, Ord, Show)

data C0Basic = C0ArgE C0Arg
             | C0PlusE C0Arg C0Arg
  deriving (Eq, Ord, Show)

data C0Stmt = AssignC0 Variable C0Basic
  deriving (Eq, Ord, Show)

data C0Tail = ReturnC0 C0Basic
            | SeqC0 C0Stmt C0Tail
  deriving (Eq, Ord, Show)

-- Compile a R1 argument (integer or variable) into a C0Arg expression
ecArg :: R1Expr -> C0Arg
ecArg e = case e of
  IntE i -> undefined
  VarE x -> undefined

-- Compile a BASIC R1 Expression into a C0Basic Expression
ecBasic :: R1Expr -> C0Basic
ecBasic e = case e of
  PlusE e1 e2 -> undefined
  _ -> undefined

-- The explicate-control pass on an expression in TAIL POSITION
-- input:  a COMPLEX EXPRESSION in A-Normal Form
-- output: a C0 Tail expression
ecTail :: R1Expr -> C0Tail
ecTail e = case e of
  IntE _ -> undefined
  VarE _ -> undefined
  PlusE e1 e2 -> undefined
  LetE x e1 e2 -> undefined

-- The explicate-control pass on an expression in ASSIGNMENT POSITION
-- input:
--   - the variable being assigned
--   - the R1 Expression it is being assigned to
--   - a C0 Tail expression describing what should happen *after* the assignment
-- output: a C0 Tail expression
ecAssign :: Variable -> R1Expr -> C0Tail -> C0Tail
ecAssign x e k = case e of
  IntE _ -> undefined
  VarE _ -> undefined
  PlusE e1 e2 -> undefined
  LetE x' e1 e2 -> undefined


------------------------------------------------------------
-- select-instructions
------------------------------------------------------------

data X86Arg = VarXE Variable
            | DerefE String Int
            | RegE String
            | IntXE Int
  deriving (Eq, Ord, Show)

data X86Instr = MovqE X86Arg X86Arg
              | AddqE X86Arg X86Arg
              | RetqE
  deriving (Eq, Ord, Show)

siArg :: C0Arg -> X86Arg
siArg e = case e of
  IntC0 i -> undefined
  VarC0 x -> undefined

-- The select-instructions pass on a C0Stmt statement
-- input:  a C0Stmt
-- output: a list of pseudo-x86 instructions
siStmt :: C0Stmt -> [X86Instr]
siStmt e = case e of
  AssignC0 x (C0ArgE a) -> undefined
  AssignC0 x (C0PlusE a1 a2) -> undefined

-- The select-instructions pass on a C0Tail expression
-- input:  a C0 Tail expression
-- output: a list of pseudo-X86 instructions
siTail :: C0Tail -> [X86Instr]
siTail e = case e of
  ReturnC0 a -> undefined
  SeqC0 s t -> undefined

------------------------------------------------------------
-- assign-homes
------------------------------------------------------------

-- Find the variables used in an x86 "arg"
varsArg :: X86Arg -> [Variable]
varsArg e = case e of
  VarXE s -> undefined
  RegE r -> undefined
  IntXE i -> undefined

-- Find the variables used in an x86 instruction
varsInstr :: X86Instr -> [Variable]
varsInstr e = case e of
  MovqE a1 a2 -> undefined
  AddqE a1 a2 -> undefined
  RetqE -> []

-- Given an integer offset and a variable name,
-- map the variable to a memory location on the stack
-- (i.e. give the variable a "home")
mkStackLoc :: (Int, Variable) -> (Variable, X86Arg)
mkStackLoc (i, x) = (x, DerefE "rbp" (-8 * (i + 1)))

-- The assign-homes pass
-- input:  a list of pseudo-x86 instructions
-- output: a pair
--   - a list of x86 instructions (without variables)
--   - the number of stack locations used 
assignHomes :: [X86Instr] -> ([X86Instr], Int)
assignHomes ss =
      -- get a list of variable names without duplicates
  let localVariables   = nub $ concat (map varsInstr ss)
      -- assign each variable a location on the stack
      stackAssignments = zip [0..] localVariables
      -- make a stack location expression for each assignment
      homes            = map mkStackLoc stackAssignments
      -- replace each use of a variable with a ref to its home
      newInstructions  = map (ahInstr homes) ss
  -- return the new instructions and number of homes used
  in (newInstructions, length homes)

-- The assign-homes pass, for a single instruction
-- inputs:
--   - a mapping from variables to their "homes"
--   - a single pseudo-x86 instruction
-- output: a single x86 instruction
ahInstr :: [(Variable, X86Arg)] -> X86Instr -> X86Instr
ahInstr homes e = case e of
  MovqE a1 a2 -> undefined
  AddqE a1 a2 -> undefined
  RetqE -> undefined

-- The assign-homes pass, for a single pseudo-x86 "arg"
-- inputs:
--   - a mapping from variables to their "homes"
--   - a single pseudo-x86 "arg"
-- output: a single x86 "arg"
ahArg :: [(Variable, X86Arg)] -> X86Arg -> X86Arg
ahArg homes e = undefined

------------------------------------------------------------
-- patch-instructions
------------------------------------------------------------

-- The patch-instructions pass
-- input: a pair
--   - a list of x86 instructions
--   - the number of stack locations used
-- output: a pair
--   - a list of *patched* x86 instructions
--   - the number of stack locations used
patchInstructions :: ([X86Instr], Int) -> ([X86Instr], Int)
patchInstructions (ss, numHomes) =
  (concat $ map piInstr ss, numHomes)

-- The patch-instructions pass, for a single instruction
-- input: a pair
--   - a single x86 instruction
--   - the number of stack locations used
-- output: a pair
--   - a single *patched* x86 instruction
--   - the number of stack locations used
-- Patched instructions contain at most one memory access in each `movq` or `addq` instruction
piInstr :: X86Instr -> [X86Instr]
piInstr e = case e of
  MovqE (DerefE r1 i1) (DerefE r2 i2) -> undefined
  MovqE _ _ -> [e]
  AddqE (DerefE r1 i1) (DerefE r2 i2) -> undefined
  AddqE _ _ -> [e]
  RetqE -> [e]

------------------------------------------------------------
-- print-x86
------------------------------------------------------------

-- Set this to `True` if you're using macos
macos :: Bool
macos = False

-- Print a function or label name
-- Add a _ at the front if we're using macos
printFun :: String -> String
printFun s = case macos of
  True -> "_" ++ s
  False -> s

-- Align the size of the stack frame to `alignment` bytes
-- Input:
--   - n: the number of bytes of stack space used on the current stack frame
--   - alignment: the desired alignment (in bytes) - for x86, usually 16 bytes
-- Output: the size in bytes of the correctly aligned stack frame
align :: Int -> Int -> Int
align n alignment = case n `mod` alignment of
  0 -> n
  _ -> n + (alignment - (n `mod` alignment))

-- The printX86 pass for x86 "args"
printX86Arg :: X86Arg -> String
printX86Arg e = undefined

-- The printX86 pass for x86 instructions
printX86Instr :: X86Instr -> String
printX86Instr e = undefined

-- The printX86 pass for x86 programs
-- Input: a pair
--   - a list of instructions
--   - the number of stack locations used in the program
-- Output: x86 assembly, as a string
printX86 :: ([X86Instr], Int) -> String
printX86 (ss, numHomes) = undefined

------------------------------------------------------------
-- compile / main
------------------------------------------------------------

compile :: R1Expr -> String
compile = printX86 . patchInstructions . assignHomes . siTail . ecTail . rcoExp . uniquify

logOutput :: Show b => String -> (a -> b) -> (a -> IO b)
logOutput name f = \ x -> do
  let result = f x
  putStrLn "--------------------------------------------------"
  putStrLn $ "Output of pass " ++ name ++ ":"
  putStrLn "--------------------------------------------------"
  pPrintNoColor result
  putStrLn ""
  return result

compileLog :: R1Expr -> IO String
compileLog e =
  (logOutput "input" id) e >>=
  (logOutput "uniquify" uniquify) >>=
  (logOutput "rcoExp" rcoExp) >>=
  (logOutput "ecTail" ecTail) >>=
  (logOutput "siTail" siTail) >>=
  (logOutput "assignHomes" assignHomes) >>=
  (logOutput "patchInstructions" patchInstructions) >>=
  (logOutput "printX86" printX86)


