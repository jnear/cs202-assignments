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
import CFG
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
-- typecheck
------------------------------------------------------------

data Type = IntT | BoolT deriving (Eq, Ord, Show)

type TEnv = [(Variable, Type)]

-- Typechecker for R2 Expressions
-- Inputs:
--  - e: an R2 expression
--  - env: a type environment mapping variables to their types
-- Output: the type of e, if it is well-typed
-- Throws a run-time error if e is not well-typed
tcExpr :: R2Expr -> TEnv -> Type
tcExpr e env = undefined


-- Typechecker for R2 Expressions
typecheck :: R2Expr -> R2Expr
typecheck e =
  let t = tcExpr e []
  in trace ("Type: " ++ (show t)) e


------------------------------------------------------------
-- shrink
------------------------------------------------------------

-- Shrink, for R2 expressions
-- Input: e, an R2 expression
-- Output: an R2 expression
-- Removes "and", "or", ">=", ">", ">="
shrink :: R2Expr -> R2Expr
shrink e = undefined


------------------------------------------------------------
-- uniquify
------------------------------------------------------------

-- A Variable Environment maps variables to variables
type VEnv = [(Variable, Variable)]

-- The uniquify pass, for an expression
-- Input:
--   - an R2 expression
--   - a variable environment
-- Output: an R2 expression, with variables renamed to be unique
uniquifyExp :: R2Expr -> VEnv -> R2Expr
uniquifyExp e env = undefined

-- The uniquify pass, for an R2 program
-- Input:  an R2 expression
-- Output: an R2 expression, with variables renamed to be unique
-- This function simply calls `uniquifyExp` with the empty variable environment
uniquify :: R2Expr -> R2Expr
uniquify e = uniquifyExp e []

------------------------------------------------------------
-- remove-complex-opera
------------------------------------------------------------

-- a Binding maps a variable to an expression
type Binding = (Variable, R2Expr)

-- The remove-complex-operand pass on an expression in TAIL POSITION
-- input:  COMPLEX EXPRESSION
-- output: COMPLEX EXPRESSION in A-Normal Form
rcoExp :: R2Expr -> R2Expr
rcoExp e = undefined

-- The remove-complex-operand pass on an expression in ARGUMENT POSITION
-- input:  COMPLEX EXPRESSION
-- output: pair: SIMPLE EXPRESSION and LIST OF BINDINGS
-- 
-- the LIST OF BINDINGS maps variables to SIMPLE EXPRESSIONS
rcoArg :: R2Expr -> (R2Expr, [Binding])
rcoArg e = undefined

-- Make a "LET" expression from a list of bindings and a final "body" expression
mkLet :: [Binding] -> R2Expr -> R2Expr
mkLet [] body = body
mkLet ((x, e) : bs) body = LetE x e (mkLet bs body)

------------------------------------------------------------
-- explicate-control
------------------------------------------------------------
type Label = String

data C1Arg = IntC1 Int
           | VarC1 Variable
           | TrueC1
           | FalseC1
  deriving (Eq, Ord, Show)

data C1Cmp = CmpEqC1
           | CmpLTC1
  deriving (Eq, Ord, Show)

data C1Basic = C1ArgE C1Arg
             | C1PlusE C1Arg C1Arg
             | C1NotE C1Arg
             | C1CmpE C1Cmp C1Arg C1Arg
  deriving (Eq, Ord, Show)

data C1Stmt = AssignC1 Variable C1Basic
  deriving (Eq, Ord, Show)

data C1Tail = ReturnC1 C1Basic
            | SeqC1 C1Stmt C1Tail
            | GotoC1 Label
            | IfC1 C1Cmp C1Arg C1Arg Label Label
  deriving (Eq, Ord, Show)

--type C1CFG = [(Label, C1Tail)]

type C1CFG = CFG C1Tail

-- Compile a R2 argument (integer or variable) into a C1Arg expression
ecArg :: R2Expr -> C1Arg
ecArg e = case e of
  IntE i -> IntC1 i
  VarE x -> VarC1 x
  TrueE -> TrueC1
  FalseE -> FalseC1
  _ -> error $ show e

ecCmp :: Cmp -> C1Cmp
ecCmp c = case c of
  CmpEqual -> CmpEqC1
  CmpLT -> CmpLTC1

-- Compile a BASIC R2 Expression into a C1Basic Expression
ecBasic :: R2Expr -> C1Basic
ecBasic e = case e of
  PlusE e1 e2 -> C1PlusE (ecArg e1) (ecArg e2)
  NotE e1 -> C1NotE (ecArg e1)
  CmpE c e1 e2 -> C1CmpE (ecCmp c) (ecArg e1) (ecArg e2)
  _ -> C1ArgE (ecArg e)


-- The explicate-control pass on an expression in TAIL POSITION
-- inputs:
--  - e: a COMPLEX EXPRESSION in A-Normal Form
--  - cfg: a C1CFG (control flow graph)
-- output: a C1 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecTail :: R2Expr -> C1CFG -> C1Tail
ecTail e cfg = undefined


-- The explicate-control pass on an expression in PREDICATE POSITION
-- inputs:
--  - test: a COMPLEX R2 EXPRESSION
--  - b1: a C1 Tail expression (the "then-block")
--  - b2: a C1 Tail expression (the "else-block")
--  - cfg: a C1CFG (control flow graph)
-- output: a C1 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecPred :: R2Expr -> C1Tail -> C1Tail -> C1CFG -> C1Tail
ecPred test b1 b2 cfg = undefined

-- The explicate-control pass on an expression in ASSIGNMENT POSITION
-- input:
--   - x: the variable being assigned
--   - e: the R2 Expression it is being assigned to
--   - k: a C1 Tail expression describing what should happen *after* the assignment
--   - cfg: a C1CFG (control flow graph)
-- output: a C1 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecAssign :: Variable -> R2Expr -> C1Tail -> C1CFG -> C1Tail
ecAssign x e k cfg = undefined

-- The explicate control pass
-- input: an R2 expression
-- output: a C1 control flow graph
explicateControl :: R2Expr -> C1CFG
explicateControl e =
  let cfg = emptyCFG ()
      b = ecTail e cfg
      _ = addCFGNode cfg "start" b
  in cfg

------------------------------------------------------------
-- select-instructions
------------------------------------------------------------

data X86Arg = VarXE Variable
            | DerefE String Int
            | RegE String
            | ByteRegE String
            | IntXE Int
  deriving (Eq, Ord, Show)

data X86CC = CCe
           | CCl
           | CCle
           | CCg
           | CCge
  deriving (Eq, Ord, Show)

data X86Instr = MovqE X86Arg X86Arg
              | AddqE X86Arg X86Arg
              | RetqE
              | XorqE X86Arg X86Arg
              | CmpqE X86Arg X86Arg
              | SetE X86CC X86Arg
              | MovzbqE X86Arg X86Arg
              | JmpE Label
              | JmpIfE X86CC Label
  deriving (Eq, Ord, Show)

type X86CFG = [(Label, [X86Instr])]

-- select instructions for a C1 Arg
-- returns an x86 argument
siArg :: C1Arg -> X86Arg
siArg e = case e of
  IntC1 i -> IntXE i
  VarC1 x -> VarXE x
  TrueC1 -> IntXE 1
  FalseC1 -> IntXE 0

-- select instructions for a C1 comparison
-- returns an x86 comparison code
siCC :: C1Cmp -> X86CC
siCC c = case c of
  CmpEqC1 -> CCe
  CmpLTC1 -> CCl

-- The select-instructions pass on a C1Stmt statement
-- input:  a C1Stmt
-- output: a list of pseudo-x86 instructions
siStmt :: C1Stmt -> [X86Instr]
siStmt e = undefined

-- The select-instructions pass on a C1Tail expression
-- input:  a C1 Tail expression
-- output: a list of pseudo-X86 instructions
siTail :: C1Tail -> [X86Instr]
siTail e = undefined

-- The select-instructions pass, for a basic block
-- Input: a pair
--  - the basic block's label
--  - the basic block's code, as a C1 Tail
-- Output: a pair
--  - the basic block's label
--  - the basic block's code, as a list of pseudo-x86 instructions
siBlock :: (Label, C1Tail) -> (Label, [X86Instr])
siBlock (label, block) = (label, siTail block)

-- The select-instructions pass
-- Input: a C1 control flow graph
-- Output: an x86 control flow graph
selectInstructions :: C1CFG -> X86CFG
selectInstructions cfg = map siBlock (toListCFG cfg)

------------------------------------------------------------
-- uncover-live
------------------------------------------------------------

type LivenessResult = [(Label, [Set Variable])]

-- Returns the set of referenced variables in an arg
-- input:  an x86 Arg
-- output: a set of variables mentioned in the arg
varsArg :: X86Arg -> Set Variable
varsArg e = case e of
  VarXE x -> Set.singleton x
  DerefE _ _ -> undefined
  RegE _ -> Set.empty
  IntXE _ -> Set.empty

-- Liveness analysis, for an instruction
-- inputs:
--   - e: an instruction
--   - lAfter: the set of live-after variables for *this* instruction (e)
-- output: the set of live-before variables for *this* instruction in the program
--   in other words: the set of live-after variables for the *previous* instruction
ulInstr :: X86Instr -> Set Variable -> Set Variable
ulInstr e lAfter = undefined

-- Liveness analysis, for multiple instructions
-- input:  a list of instructions
-- output: a list of live-after sets for each instruction in the program, plus one extra
--  live-after set for the point before the first program instruction
ulInstrs :: Set Variable -> [X86Instr] -> [Set Variable]
ulInstrs lBefore [] = [lBefore]
ulInstrs lBefore (i : is) =
  let prevLafter : lAfters = ulInstrs lBefore is
      nextLafter = ulInstr i prevLafter
  in nextLafter : prevLafter : lAfters

-- Liveness analysis, for a pseudo-x86 control flow graph
-- inputs:
--  - ls: a topological sorted control flow graph
--  - cfg: a pseudo-x86 control flow graph
--  - lBefores: for each label, the live-before set for the associated basic block
-- output: a liveness analysis result
--  represented as a mapping from label to a list of live-after sets
ulBlocks :: [(Label, Set Label)] -> X86CFG -> [(Label, Set Variable)] -> LivenessResult
ulBlocks [] cfg lBefores = []
ulBlocks ((l, adj) : ls) cfg lBefores =
  let lBefore :: Set Label = Set.unions (Set.map (\dest -> fromJust (lookup dest lBefores)) adj)
      (lB : lAS) = ulInstrs lBefore (fromJust (lookup l cfg))
      newLBefores = (l, lB) : lBefores
  in (l, lAS) : ulBlocks ls cfg newLBefores

-- Liveness analysis, for a pseudo-x86 control flow graph
-- input:  a pseudo-x86 control flow graph
-- output: a pair:
--   1. a liveness result (a mapping from labels to lists of live-after sets)
--   2. a pseudo-x86 control flow graph
-- How it works:
--  1. For each label in the CFG, find the adjacent blocks
--  2. Perform a topological sort on the directed graph representing the result in #1
--  3. Sort the CFG according to #2
--  4. Initialize the live-before set for each label to be empty
--  5. Run the liveness analysis on the sorted CFG in order
uncoverLive :: X86CFG -> (LivenessResult, X86CFG)
uncoverLive cfg =
  let adjacentBlockSets = map adjacentBlocks cfg
      sortedLabels = tsort adjacentBlockSets
      sortedCFG = map (\l -> (l, fromJust (lookup l adjacentBlockSets))) sortedLabels
      initLiveBefores = map (\l -> (l, Set.empty)) sortedLabels
      lAfters = ulBlocks sortedCFG cfg initLiveBefores
  in (lAfters, cfg)

-- topological sort
tsort :: [(Label, Set Label)] -> [Label]
tsort [] = []
tsort ls =
  let p@(l, _) = case find (\(l, s) -> Set.null s) ls of
        Nothing -> error ("tsort failure: \n" ++ (T.unpack $ pShowNoColor ls))
        Just x -> x
      ls2 = delete p ls
      ls3 = map (\(l', s) -> (l', Set.delete l s)) ls2
  in l : tsort ls3

-- Find the blocks adjacent to a block
adjacentBlocks :: (Label, [X86Instr]) -> (Label, Set Label)
adjacentBlocks (label, instrs) =
  (label, Set.unions (map adjacentBlocksInstr instrs))

-- Find the blocks an instruction jumps to
adjacentBlocksInstr :: X86Instr -> Set Label
adjacentBlocksInstr i = case i of
  JmpE l -> Set.singleton l
  JmpIfE _ l -> Set.singleton l
  _ -> Set.empty


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
biInstrs :: [(X86Instr, Set Variable)] -> Graph Variable -> Graph Variable
biInstrs [] g = g
biInstrs (p : ps) g = biInstrs ps (biInstr p g)

-- build-interference, for a block
-- inputs:
--  - l: the block's label
--  - cfg: the x86 control flow graph
--  - liveness: the liveness analysis results for the cfg
--  - g: the current interference graph
-- output: the updated interference graph
biBlock :: Label -> X86CFG -> LivenessResult -> Graph Variable -> Graph Variable
biBlock l cfg liveness g =
  let instrs = fromJust $ lookup l cfg
      las = fromJust $ lookup l liveness
  in biInstrs (zip instrs las) g

-- build-interference, for multiple blocks
-- inputs:
--  - ls: the labels of multiple blocks
--  - cfg: the x86 control flow graph
--  - liveness: the liveness analysis results for the cfg
--  - g: the current interference graph
-- output: the updated interference graph
biBlocks :: [Label] -> X86CFG -> LivenessResult -> Graph Variable -> Graph Variable
biBlocks [] _ _ g = g
biBlocks (l : ls) cfg liveness g = biBlocks ls cfg liveness (biBlock l cfg liveness g)

-- build-interference, for a pseudo-x86 program
-- input:  a pair:
--  - the liveness analysis results for the program
--  - the pseudo-x86 control flow graph for the program
-- output: a pair:
--  - the complete interference graph
--  - the pseudo-x86 control flow graph
buildInterference :: (LivenessResult, X86CFG) -> (Graph Variable, X86CFG)
buildInterference (liveness, cfg) =
  let labels = map fst cfg
      g = biBlocks labels cfg liveness emptyGraph
  in (g, cfg)

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
varsInstr e = undefined

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
allocateRegisters :: (Graph Variable, X86CFG) -> (X86CFG, Int)
allocateRegisters (g, cfg) =
  let locals    :: [Variable]           = nub (concat (map (getLocalVars . snd) cfg))
      coloring  :: Coloring             = colorGraph locals g Map.empty
      allColors :: [Color]              = [0 .. length locals]
      (regColors, stackColors)          = splitAt (length registerArgs) allColors
      regMap    :: [(Int, X86Arg)]      = zip regColors registerArgs
      stackMap  :: [(Int, X86Arg)]      = map mkStackColor (zip stackColors [0 .. length stackColors])
      colorMap  :: [(Int, X86Arg)]      = regMap ++ stackMap
      homes     :: [(Variable, X86Arg)] = map (getHome coloring colorMap) locals
      cfg'      :: X86CFG               = map (ahBlock homes) cfg
  in trace ("\nColoring: " ++ (show coloring) ++
            "\nHomes: " ++ (show homes) )
  (cfg', length stackColors)

-- assign-homes for a block
ahBlock :: [(Variable, X86Arg)] -> (Label, [X86Instr]) -> (Label, [X86Instr])
ahBlock homes (l, instrs) = (l, map (ahInstr homes) instrs)

-- copied from assign-homes
ahInstr :: [(Variable, X86Arg)] -> X86Instr -> X86Instr
ahInstr homes e = undefined
  
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
patchInstructions :: (X86CFG, Int) -> (X86CFG, Int)
patchInstructions (cfg, numHomes) = (map piBlock cfg, numHomes)

-- The patch-instructions pass, for a basic block
-- input: a pair:
--  - the label of the block
--  - the list of instructions for the block
-- output: a pair:
--  - the label of the block
--  - the list of (updated) instructions for the block
piBlock :: (Label, [X86Instr]) -> (Label, [X86Instr])
piBlock (l, instrs) = (l, concat (map piInstr instrs))

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
  MovqE (DerefE r1 i1) (DerefE r2 i2) -> [ MovqE (DerefE r1 i1) (RegE "rax")
                                         , MovqE (RegE "rax") (DerefE r2 i2) ]
  MovqE _ _ -> [e]
  AddqE (DerefE r1 i1) (DerefE r2 i2) -> [ MovqE (DerefE r1 i1) (RegE "rax")
                                         , AddqE (RegE "rax") (DerefE r2 i2) ]
  AddqE _ _ -> [e]
  RetqE -> [e]
  CmpqE a1 (IntXE i) -> [ MovqE (IntXE i) (RegE "rax")
                        , CmpqE a1 (RegE "rax") ]
  CmpqE _ _ -> [e]
  JmpIfE _ _ -> [e]
  JmpE _ -> [e]
  SetE _ (ByteRegE _) -> [e]
  MovzbqE (ByteRegE r) (DerefE r1 i1) -> [ MovzbqE (ByteRegE r) (RegE "rax")
                                         , MovqE (RegE "rax") (DerefE r1 i1) ]
  MovzbqE _ _ -> [e]
  XorqE (DerefE r1 i1) (DerefE r2 i2) -> [ MovqE (DerefE r1 i1) (RegE "rax")
                                         , XorqE (RegE "rax") (DerefE r2 i2) ]
  XorqE _ _ -> [e]
  _ -> error (show e)

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
printX86Arg e = case e of
  VarXE s -> "%%" ++ s
  RegE r -> "%" ++ r
  IntXE i -> "$" ++ (show i)
  DerefE r i -> (show i) ++ "(%" ++ r ++ ")"
  ByteRegE r -> "%" ++ r

-- The printX86 pass for x86 instructions
printX86Instr :: X86Instr -> String
printX86Instr e = undefined

printX86Block :: (Label, [X86Instr]) -> String
printX86Block (l, instrs) = (printFun l) ++ ":\n" ++ (intercalate "\n" $ map printX86Instr instrs)

-- The printX86 pass for x86 programs
-- Input: a pair
--   - a list of instructions
--   - the number of stack locations used in the program
-- Output: x86 assembly, as a string
printX86 :: (X86CFG, Int) -> String
printX86 (cfg, numHomes) =
  let stackSize = align (8 * numHomes) 16 in
  " .globl " ++ (printFun "main") ++ "\n" ++
  (printFun "main") ++ ":\n" ++
  " pushq %rbp\n" ++
  " movq %rsp, %rbp\n" ++
  " subq $" ++ (show stackSize) ++ ", %rsp\n" ++
  " jmp " ++ (printFun "start") ++ "\n" ++
  (intercalate "\n" $ map printX86Block cfg) ++ "\n" ++
  (printFun "conclusion") ++ ":\n" ++
  " movq %rax, %rdi\n" ++
  " callq " ++ (printFun "print_int") ++ "\n" ++
  " movq $0, %rax\n" ++
  " addq $" ++ (show stackSize) ++ ", %rsp\n" ++
  " popq %rbp\n" ++
  " retq\n"

------------------------------------------------------------
-- compile / main
------------------------------------------------------------

compile :: R2Expr -> String
compile = printX86 . patchInstructions . allocateRegisters . buildInterference .
  uncoverLive . selectInstructions . explicateControl . rcoExp . uniquify . shrink . typecheck

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

compileLog :: R2Expr -> IO String
compileLog e =
  (logOutput "input" id) e >>=
  (logOutput "typecheck" typecheck) >>=
  (logOutput "shrink" shrink) >>=
  (logOutput "uniquify" uniquify) >>=
  (logOutput "rcoExp" rcoExp) >>=
  (logOutput "explicateControl" explicateControl) >>=
  (logOutput "selectInstructions" selectInstructions) >>=
  (logOutput "uncoverLive" uncoverLive) >>=
  (logOutput "buildInterference" buildInterference) >>=
  (logOutput "allocateRegisters" allocateRegisters) >>=
  (logOutput "patchInstructions" patchInstructions) >>=
  (logOutput "printX86" printX86)
