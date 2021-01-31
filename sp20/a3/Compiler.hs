{-# LANGUAGE Strict, ScopedTypeVariables #-}
module Compiler where

import Data.List
import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord

import Text.Pretty.Simple (pPrint, pPrintNoColor, pShowNoColor)

import qualified Data.Text.Lazy as T

import Graph
import Gensym
import AST

-- a Type Alias for variables
-- Variables are represented by Strings
type Variable = String

------------------------------------------------------------
-- register defintions
------------------------------------------------------------

callerSavedRegisters = ["rdx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "r11"]
calleeSavedRegisters = ["rbx", "r12", "r13", "r14", "r15"]

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
ecArg e = undefined

-- Compile a BASIC R1 Expression into a C0Basic Expression
ecBasic :: R1Expr -> C0Basic
ecBasic e = undefined

-- The explicate-control pass on an expression in TAIL POSITION
-- input:  a COMPLEX EXPRESSION in A-Normal Form
-- output: a C0 Tail expression
ecTail :: R1Expr -> C0Tail
ecTail e = undefined

-- The explicate-control pass on an expression in ASSIGNMENT POSITION
-- input:
--   - the variable being assigned
--   - the R1 Expression it is being assigned to
--   - a C0 Tail expression describing what should happen *after* the assignment
-- output: a C0 Tail expression
ecAssign :: Variable -> R1Expr -> C0Tail -> C0Tail
ecAssign x e k = undefined


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
siArg e = undefined

-- The select-instructions pass on a C0Stmt statement
-- input:  a C0Stmt
-- output: a list of pseudo-x86 instructions
siStmt :: C0Stmt -> [X86Instr]
siStmt e = undefined

-- The select-instructions pass on a C0Tail expression
-- input:  a C0 Tail expression
-- output: a list of pseudo-X86 instructions
siTail :: C0Tail -> [X86Instr]
siTail e = undefined

------------------------------------------------------------
-- uncover-live
------------------------------------------------------------

-- Returns the set of referenced variables in an arg
-- input:  an x86 Arg
-- output: a set of variables mentioned in the arg
varsArg :: X86Arg -> Set Variable
varsArg e = undefined

-- Liveness analysis, for an instruction
-- inputs:
--   - e: an instruction
--   - prevLiveAfter: the set of live-after variables for *this* instruction (e)
-- output: the set of live-after variables for the *previous* instruction in the program
ulInstr :: X86Instr -> Set Variable -> Set Variable
ulInstr e lAfter = undefined

-- Liveness analysis, for multiple instructions
-- input:  a list of instructions
-- output: a list of live-after sets for each instruction in the program, plus one extra
--  live-after set for the point before the first program instruction
ulInstrs :: [X86Instr] -> [Set Variable]
ulInstrs [] = [Set.empty]
ulInstrs (i : is) =
  let prevLafter : lAfters = ulInstrs is
      nextLafter = ulInstr i prevLafter
  in nextLafter : prevLafter : lAfters

-- Liveness analysis, for a list of instructions
-- input:  a list of instructions
-- output: a list of pairs:
--   1. an instruction
--   2. the set of live-after variables for that instruction
-- note: we throw out the first result from ulInstrs (it is not a live-after set for any instruction)
uncoverLive :: [X86Instr] -> [(X86Instr, Set Variable)]
uncoverLive instrs =
  let (_ : lAfters) = ulInstrs instrs
  in zip instrs lAfters

------------------------------------------------------------
-- build-interference
------------------------------------------------------------

-- Add edges between one variable and a list of other variables
-- input:
--  - an interference graph (g)
--  - a "destination" variable (d)
--  - a list of "source" variables
-- output: a new interference graph, with edges between d and each
--  variable in the list of source variables
addEdges :: Graph Variable -> Variable -> [Variable] -> Graph Variable
addEdges g d []       = g
addEdges g d (v : vs) =
  let g' = addEdges g d vs
  in addEdge g' d v

-- build-interference, for a single instruction
-- input:
--  - a pair, containing an instruction and the live-after set for that instruction
--  - the current interference graph
-- output:
--  - a new interference graph
biInstr :: (X86Instr, Set Variable) -> Graph Variable -> Graph Variable
biInstr (instr, liveAfter) g = undefined

-- build-interference, for a list of instructions
-- input:  a list of pairs, each one containing an instruction and live-after set
-- output: a complete interference graph
biInstrs :: [(X86Instr, Set Variable)] -> Graph Variable
biInstrs [] = emptyGraph
biInstrs (p : ps) = 
  let g = biInstrs ps
  in biInstr p g

-- build-interference, for a pseudo-x86 program
-- input:  a list of pairs, each one containing an instruction and live-after set
-- output: a pair, containing the complete interference graph and the list of instructions
buildInterference :: [(X86Instr, Set Variable)] -> (Graph Variable, [X86Instr])
buildInterference ps =
  let g = biInstrs ps
  in (g, map fst ps)

------------------------------------------------------------
-- allocate-registers
------------------------------------------------------------

-- Our "colors" will be represented by integers
type Color = Int

-- A "coloring" for a graph is a mapping from variables in the graph to colors
type Coloring = Map Variable Color

-- A "saturation" is the set of colors used by neighboring nodes in the graph
type Saturation = Set Color

-- pick a color that isn't already used by a neighbor
-- input:  a saturation
-- output: a color
pickColor :: Saturation -> Color
pickColor saturation = unusedColor saturation 0

-- find the "smallest" color not in a saturation
-- input:  saturation (sat) and lowest color to consider (c)
-- output: lowest color not present in "sat"
unusedColor :: Saturation -> Int -> Int
unusedColor sat c | Set.member c sat = unusedColor sat (c+1)
                  | otherwise = c

-- get the colors assigned to a list of variables in the coloring assignment
-- input:
--  - a coloring (cs)
--  - a list of variables
-- output: a set of colors, containing all the colors assigned to variables in the
--  list by the coloring "cs"
getColors :: Coloring -> [Variable] -> Set Color
getColors cs [] = Set.empty
getColors cs (x : xs) | Map.member x cs = Set.insert (cs Map.! x) (getColors cs xs)
                      | otherwise       = getColors cs xs

-- get the saturation of a node in the interference graph
-- input:
--  - a variable (x)
--  - a coloring (cs)
--  - an interference graph (g)
-- output: the saturation of x given cs and g
getSaturation :: Variable -> Coloring -> Graph Variable -> Saturation
getSaturation x cs g =
  let ns = Set.toList (neighbors g x)
  in getColors cs ns

-- helper to return the index of the maximum value in a list
maxi :: (Ord a) => [a] -> (a, Int)
maxi xs = maximumBy (comparing fst) (zip xs [0..])

-- find the maximally saturated variable in a list of variables
-- input:
--  - a list of variables (xs)
--  - the current coloring (cs)
--  - the interference graph (g)
-- output:
--  - the variable from xs with the maximum saturation, given cs and g
maxSat :: [Variable] -> Coloring -> Graph Variable -> Variable
maxSat xs cs g =
  let sats   = map (\x -> getSaturation x cs g) xs
      sizes  = map Set.size sats
      (_, i) = maxi sizes
  in xs !! i

-- color the graph
-- input:
--  - a list of variables (xs) to color
--  - the interference graph (g)
--  - the current coloring (cs)
-- output:
--  - the new coloring
colorGraph :: [Variable] -> Graph Variable -> Coloring -> Coloring
colorGraph [] g cs = cs
colorGraph xs g cs = undefined

-- get the variables used in a program
-- input: a list of instructions
-- output: a list of variables
getLocalVars :: [X86Instr] -> [Variable]
getLocalVars instrs = Set.toList (Set.unions (map varsInstr instrs))

varsInstr :: X86Instr -> Set Variable
varsInstr e = case e of
  MovqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  AddqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  RetqE -> Set.empty

-- a list of all the registers available to use
registerArgs = map RegE (callerSavedRegisters ++ calleeSavedRegisters)

-- given a color and offset on the stack, return a stack location assigned to the color
mkStackColor :: (Int, Int) -> (Int, X86Arg)
mkStackColor (color, i) = (color, DerefE "rbp" (-8 * (i + 1)))

-- given a coloring, a map from colors to their locations (on the stack or in a register),
-- and a variable, return the assigned location for that variable
-- input:
--  - a coloring (cs)
--  - a list of locations (locs) (pairing colors with locations)
--  - a variable (x)
-- output: the location for the variable "x"
getHome :: Coloring -> [(Color, X86Arg)] -> Variable -> (Variable, X86Arg)
getHome cs locs x = undefined

-- the allocate-registers pass
-- input:  a pair, containing an interference graph and list of pseudo-x86 instructions
-- output: a pair, containing a list of x86 instructions and the number of stack locations used
allocateRegisters :: (Graph Variable, [X86Instr]) -> ([X86Instr], Int)
allocateRegisters (g, instrs) =
  let locals    :: [Variable]           = getLocalVars instrs
      coloring  :: Coloring             = colorGraph locals g Map.empty
      allColors :: [Color]              = [0 .. length locals]
      (regColors, stackColors)          = splitAt (length registerArgs) allColors
      regMap    :: [(Int, X86Arg)]      = zip regColors registerArgs
      stackMap  :: [(Int, X86Arg)]      = map mkStackColor (zip stackColors [0 .. length stackColors])
      colorMap  :: [(Int, X86Arg)]      = regMap ++ stackMap
      homes     :: [(Variable, X86Arg)] = map (getHome coloring colorMap) locals
      instrs'   :: [X86Instr]           = map (ahInstr homes) instrs
  in trace ("\nColoring: " ++ (show coloring) ++
            "\nHomes: " ++ (show homes) )
  (instrs', length stackColors)

-- copied from assign-homes
ahInstr :: [(String, X86Arg)] -> X86Instr -> X86Instr
ahInstr homes e = undefined
  
ahArg :: [(String, X86Arg)] -> X86Arg -> X86Arg
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
piInstr e = undefined

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
compile = printX86 . patchInstructions . allocateRegisters . buildInterference .
  uncoverLive . siTail . ecTail . rcoExp . uniquify

logOutput :: Show b => String -> (a -> b) -> (a -> IO b)
logOutput name f = \ x -> do
  putStrLn "--------------------------------------------------"
  putStrLn $ "Output of pass " ++ name ++ ":"
  putStrLn "--------------------------------------------------"
  let result = f x
  putStrLn ""
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
  (logOutput "uncoverLive" uncoverLive) >>=
  (logOutput "buildInterference" buildInterference) >>=
  (logOutput "allocateRegisters" allocateRegisters) >>=
  (logOutput "patchInstructions" patchInstructions) >>=
  (logOutput "printX86" printX86)



