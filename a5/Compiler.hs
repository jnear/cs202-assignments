{-# LANGUAGE Strict, ScopedTypeVariables #-}
module Compiler where

import Data.List
import Data.Maybe
import qualified Data.Bits as Bits

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
-- global constant definitions
------------------------------------------------------------

-- r11 and r15 are reserved for vector operations
callerSavedRegisters = ["rdx", "rcx", "rsi", "rdi", "r8", "r9", "r10"]
calleeSavedRegisters = ["rbx", "r12", "r13", "r14"]
allRegisters = callerSavedRegisters ++ calleeSavedRegisters

-- size of the root stack and heap
rootStackSize = 2^14
heapSize = 2^4

------------------------------------------------------------
-- typecheck
------------------------------------------------------------

type TEnv = [(Variable, Type)]

-- Typechecker for R3 Expressions
-- Inputs:
--  - e: an R3 expression
--  - env: a type environment mapping variables to their types
-- Output: a pair
--  - the type of e, if it is well-typed
--  - a *typed* R3 expression
-- Throws a run-time error if e is not well-typed
tcExpr :: R3Expr -> TEnv -> (Type, TypedR3Expr)
tcExpr e env = undefined

-- Typechecker for R3 Expressions
typecheck :: R3Expr -> TypedR3Expr
typecheck e =
  let (t, e') = tcExpr e []
  in trace ("Type: " ++ (show t)) e'


------------------------------------------------------------
-- shrink
------------------------------------------------------------

-- Shrink, for R3 expressions
-- Input: e, an R3 expression
-- Output: an R3 expression
-- Removes "and", "or", ">=", ">", ">="
shrink :: TypedR3Expr -> TypedR3Expr
shrink e = undefined


------------------------------------------------------------
-- uniquify
------------------------------------------------------------

-- A Variable Environment maps variables to variables
type VEnv = [(Variable, Variable)]

-- The uniquify pass, for an expression
-- Input:
--   - an R3 expression
--   - a variable environment
-- Output: an R3 expression, with variables renamed to be unique
uniquifyExp :: TypedR3Expr -> VEnv -> TypedR3Expr
uniquifyExp e env = undefined

-- The uniquify pass, for an R3 program
-- Input:  an R3 expression
-- Output: an R3 expression, with variables renamed to be unique
-- This function simply calls `uniquifyExp` with the empty variable environment
uniquify :: TypedR3Expr -> TypedR3Expr
uniquify e = uniquifyExp e []


------------------------------------------------------------
-- expose-allocation
------------------------------------------------------------

-- a Binding maps a variable to a typed expression
type Binding = (Variable, TypedR3Expr)

-- Make a "LET" expression from a list of bindings and a final "body" expression
mkLet :: [Binding] -> TypedR3Expr -> TypedR3Expr
mkLet [] body = body
mkLet ((x, e) : bs) body = LetTE x e (mkLet bs body)

-- Expose allocation, for an R3 expression
-- Input: an R3 expression
-- Output: an R3 expression, without "VectorTE" expressions
-- This pass compiles "VectorTE" expressions into explicit allocations
eaExp :: TypedR3Expr -> TypedR3Expr
eaExp e = undefined

-- Expose allocation, for an R3 expression
-- Input: an R3 expression
-- Output: an R3 expression, without "VectorTE" expressions
exposeAllocation :: TypedR3Expr -> TypedR3Expr
exposeAllocation e = eaExp e

------------------------------------------------------------
-- remove-complex-opera
------------------------------------------------------------

-- The remove-complex-operand pass on an expression in TAIL POSITION
-- input:  COMPLEX EXPRESSION
-- output: COMPLEX EXPRESSION in A-Normal Form
rcoExp :: TypedR3Expr -> TypedR3Expr
rcoExp e = undefined


-- The remove-complex-operand pass on an expression in ARGUMENT POSITION
-- input:  COMPLEX EXPRESSION
-- output: pair: SIMPLE EXPRESSION and LIST OF BINDINGS
-- 
-- the LIST OF BINDINGS maps variables to SIMPLE EXPRESSIONS
rcoArg :: TypedR3Expr -> (TypedR3Expr, [Binding])
rcoArg e = undefined

------------------------------------------------------------
-- explicate-control
------------------------------------------------------------
type Label = String

data C2Arg = IntC2 Int
           | VarC2 Variable Type
           | TrueC2
           | FalseC2
           | VoidC2
           | GlobalValC2 String
  deriving (Eq, Ord, Show)

data C2Cmp = CmpEqC2
           | CmpLTC2
  deriving (Eq, Ord, Show)

data C2Basic = C2ArgE C2Arg
             | C2PlusE C2Arg C2Arg
             | C2NotE C2Arg
             | C2CmpE C2Cmp C2Arg C2Arg
             | C2AllocateE Int Type
             | C2VectorRefE C2Arg Int
             | C2VectorSetE C2Arg Int C2Arg
  deriving (Eq, Ord, Show)

data C2Stmt = AssignC2 Variable Type C2Basic
            | CollectC2 Int
  deriving (Eq, Ord, Show)

data C2Tail = ReturnC2 C2Basic
            | SeqC2 C2Stmt C2Tail
            | GotoC2 Label
            | IfC2 C2Cmp C2Arg C2Arg Label Label
  deriving (Eq, Ord, Show)

type C2CFG = CFG C2Tail

-- Compile a R3 argument (integer or variable) into a C2Arg expression
ecArg :: TypedR3Expr -> C2Arg
ecArg e = case e of
  IntTE i -> IntC2 i
  VarTE x t -> VarC2 x t
  TrueTE -> TrueC2
  FalseTE -> FalseC2
  VoidTE -> VoidC2
  GlobalValTE s -> GlobalValC2 s
  _ -> error (show e)

ecCmp :: Cmp -> C2Cmp
ecCmp c = case c of
  CmpEqual -> CmpEqC2
  CmpLT -> CmpLTC2

-- Compile a BASIC R3 Expression into a C2Basic Expression
ecBasic :: TypedR3Expr -> C2Basic
ecBasic e = case e of
  PlusTE e1 e2 -> C2PlusE (ecArg e1) (ecArg e2)
  NotTE e1 -> C2NotE (ecArg e1)
  CmpTE c e1 e2 -> C2CmpE (ecCmp c) (ecArg e1) (ecArg e2)
  VectorRefTE e1 i t -> C2VectorRefE (ecArg e1) i
  VectorSetTE e1 i e2 -> C2VectorSetE (ecArg e1) i (ecArg e2)
  AllocateTE i t -> C2AllocateE i t
  _ -> C2ArgE (ecArg e)


-- The explicate-control pass on an expression in TAIL POSITION
-- inputs:
--  - e: a COMPLEX EXPRESSION in A-Normal Form
--  - cfg: a C2CFG (control flow graph)
-- output: a C2 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecTail :: TypedR3Expr -> C2CFG -> C2Tail
ecTail e cfg = undefined


-- The explicate-control pass on an expression in PREDICATE POSITION
-- inputs:
--  - test: a COMPLEX R3 EXPRESSION
--  - b1: a C2 Tail expression (the "then-block")
--  - b2: a C2 Tail expression (the "else-block")
--  - cfg: a C2CFG (control flow graph)
-- output: a C2 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecPred :: TypedR3Expr -> C2Tail -> C2Tail -> C2CFG -> C2Tail
ecPred test b1 b2 cfg = undefined

-- The explicate-control pass on an expression in ASSIGNMENT POSITION
-- input:
--   - x: the variable being assigned
--   - e: the R3 Expression it is being assigned to
--   - k: a C2 Tail expression describing what should happen *after* the assignment
--   - cfg: a C2CFG (control flow graph)
-- output: a C2 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecAssign :: Variable -> TypedR3Expr -> C2Tail -> C2CFG -> C2Tail
ecAssign x e k cfg = undefined

-- The explicate control pass
-- input: an R3 expression
-- output: a C2 control flow graph
explicateControl :: TypedR3Expr -> C2CFG
explicateControl e =
  let cfg = emptyCFG ()
      b = ecTail e cfg
      _ = addCFGNode cfg "start" b
  in cfg


------------------------------------------------------------
-- select-instructions
------------------------------------------------------------

data X86Arg = VarXE Variable Type
            | DerefE String Int
            | RegE String
            | ByteRegE String
            | IntXE Int
            | GlobalValXE String
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
              | CallqE Label
  deriving (Eq, Ord, Show)

type X86CFG = [(Label, [X86Instr])]

-- select instructions for a C2 Arg
-- returns an x86 argument
siArg :: C2Arg -> X86Arg
siArg e = case e of
  IntC2 i -> IntXE i
  VarC2 x t -> VarXE x t
  TrueC2 -> IntXE 1
  FalseC2 -> IntXE 0
  VoidC2 -> IntXE 0
  GlobalValC2 s -> GlobalValXE s

-- select instructions for a C2 comparison
-- returns an x86 comparison code
siCC :: C2Cmp -> X86CC
siCC c = case c of
  CmpEqC2 -> CCe
  CmpLTC2 -> CCl

-- Build a pointer mask for a list of types
-- Reference section 5.2.2 in the textbook
-- Input: a list of types
-- Output: a pointer mask, in decimal representation
mkPointerMask :: [Type] -> Int
mkPointerMask [] = 0
mkPointerMask (VectorT _ : ts) =
  let m = mkPointerMask ts
  in 1 + (Bits.shift m 1)
mkPointerMask (_ : ts) =
  let m = mkPointerMask ts
  in Bits.shift m 1

-- Build a tag for a vector
-- Reference section 5.2.2 in the textbook
-- Input: a list of types
-- Output: a vector tag, in decimal representation
mkTag :: [Type] -> Int
mkTag ts = undefined

-- The select-instructions pass on a C2Stmt statement
-- input:  a C2Stmt
-- output: a list of pseudo-x86 instructions
siStmt :: C2Stmt -> [X86Instr]
siStmt e = undefined

-- The select-instructions pass on a C2Tail expression
-- input:  a C2 Tail expression
-- output: a list of pseudo-X86 instructions
siTail :: C2Tail -> [X86Instr]
siTail e = undefined

-- The select-instructions pass, for a basic block
-- Input: a pair
--  - the basic block's label
--  - the basic block's code, as a C2 Tail
-- Output: a pair
--  - the basic block's label
--  - the basic block's code, as a list of pseudo-x86 instructions
siBlock :: (Label, C2Tail) -> (Label, [X86Instr])
siBlock (label, block) = (label, siTail block)

-- The select-instructions pass
-- Input: a C2 control flow graph
-- Output: an x86 control flow graph
selectInstructions :: C2CFG -> X86CFG
selectInstructions cfg = map siBlock (toListCFG cfg)

------------------------------------------------------------
-- uncover-live
------------------------------------------------------------

-- Typed variables: a pair of a variable and a type
type TypedVariable = (Variable, Type)
type LivenessResult = [(Label, [Set TypedVariable])]

-- Returns the set of referenced variables in an arg
-- input:  an x86 Arg
-- output: a set of variables mentioned in the arg
varsArg :: X86Arg -> Set TypedVariable
varsArg e = case e of
  VarXE x t -> Set.singleton (x, t)
  DerefE _ _ -> Set.empty
  RegE _ -> Set.empty
  IntXE _ -> Set.empty
  GlobalValXE _ -> Set.empty
  _ -> error $ show e

-- Liveness analysis, for an instruction
-- inputs:
--   - e: an instruction
--   - lAfter: the set of live-after variables for *this* instruction (e)
-- output: the set of live-before variables for *this* instruction in the program
--   in other words: the set of live-after variables for the *previous* instruction
ulInstr :: X86Instr -> Set TypedVariable -> Set TypedVariable
ulInstr e lAfter = undefined

-- Liveness analysis, for multiple instructions
-- input:  a list of instructions
-- output: a list of live-after sets for each instruction in the program, plus one extra
--  live-after set for the point before the first program instruction
ulInstrs :: Set TypedVariable -> [X86Instr] -> [Set TypedVariable]
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
ulBlocks :: [(Label, Set Label)] -> X86CFG -> [(Label, Set TypedVariable)] -> LivenessResult
ulBlocks [] cfg lBefores = []
ulBlocks ((l, adj) : ls) cfg lBefores =
  let lBefore :: Set TypedVariable =
        Set.unions (Set.map (\dest -> fromJust (lookup dest lBefores)) adj)
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

-- a list of all the registers available to use
registerArgs = map RegE allRegisters
callerSavedArgs = map RegE callerSavedRegisters

-- Add edges between one location (as an X86Arg) and a list of other variables
-- input:
--  - an interference graph (g)
--  - a "destination" (d)
--  - a list of "source" variables
-- output: a new interference graph, with edges between d and each
--  variable in the list of source variables
addEdges :: Graph X86Arg -> X86Arg -> [TypedVariable] -> Graph X86Arg
addEdges g d []             = g
addEdges g d ((v, tv) : vs) =
  let g' = addEdges g d vs
  in addEdge g' d (VarXE v tv)

-- Add edges between a list of locations (as X86Args) and a list of other variables
-- input:
--  - an interference graph (g)
--  - a list of "destinations"
--  - a list of "source" variables
-- output: a new interference graph, with edges between d and each
--  variable in the list of source variables
addRegisterEdges :: Graph X86Arg -> [X86Arg] -> [TypedVariable] -> Graph X86Arg
addRegisterEdges g []       vs  = g
addRegisterEdges g (r : rs) vs  =
  let g' = addRegisterEdges g rs vs
  in addEdges g' r vs

-- Determine if a typed variable is a vector or not
isVecVar :: TypedVariable -> Bool
isVecVar (_, VectorT _) = True
isVecVar _ = False

-- build-interference, for a single instruction
-- input:
--  - a pair, containing an instruction and the live-after set for that instruction
--  - the current interference graph
-- output:
--  - a new interference graph
biInstr :: (X86Instr, Set TypedVariable) -> Graph X86Arg -> Graph X86Arg
biInstr (instr, liveAfter) g = undefined

-- build-interference, for a list of instructions
-- input:  a list of pairs, each one containing an instruction and live-after set
-- output: a complete interference graph
biInstrs :: [(X86Instr, Set TypedVariable)] -> Graph X86Arg -> Graph X86Arg
biInstrs [] g = g
biInstrs (p : ps) g = biInstrs ps (biInstr p g)

-- build-interference, for a block
-- inputs:
--  - l: the block's label
--  - cfg: the x86 control flow graph
--  - liveness: the liveness analysis results for the cfg
--  - g: the current interference graph
-- output: the updated interference graph
biBlock :: Label -> X86CFG -> LivenessResult -> Graph X86Arg -> Graph X86Arg
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
biBlocks :: [Label] -> X86CFG -> LivenessResult -> Graph X86Arg -> Graph X86Arg
biBlocks [] _ _ g = g
biBlocks (l : ls) cfg liveness g = biBlocks ls cfg liveness (biBlock l cfg liveness g)

-- build-interference, for a pseudo-x86 program
-- input:  a pair:
--  - the liveness analysis results for the program
--  - the pseudo-x86 control flow graph for the program
-- output: a pair:
--  - the complete interference graph
--  - the pseudo-x86 control flow graph
buildInterference :: (LivenessResult, X86CFG) -> (Graph X86Arg, X86CFG)
buildInterference (liveness, cfg) =
  let labels = map fst cfg
      g = biBlocks labels cfg liveness emptyGraph
  in (g, cfg)

------------------------------------------------------------
-- allocate-registers
------------------------------------------------------------

-- Colors are one of:
--  - Register colors
--  - Stack location colors
--  - Root stack location colors
data Color = RegColor String
           | StackColor Int
           | RootStackColor Int
  deriving (Eq, Ord, Show)

-- A "coloring" for a graph is a mapping from locations (variables or registers)
-- in the graph to colors
type Coloring = Map X86Arg Color

-- A "saturation" is the set of colors used by neighboring nodes in the graph
type Saturation = Set Color

-- We pre-assign each register a color
regColors = map RegColor allRegisters
regColorSet = Set.fromList regColors

-- Attempt to pick a register color which does *not* occur in a saturation set
-- Input: a saturation set (sat)
-- Output: (Maybe) a register color
--  If a register color is available, return the first available color
--  If all register colors are present in the saturation set, return Nothing
pickRegColor :: Saturation -> Maybe Color
pickRegColor sat = case Set.toList (Set.difference regColorSet sat) of
  [] -> Nothing
  r : rs -> Just r

-- pick a color that isn't already used by a neighbor
-- inputs:
--  - a saturation set (sat)
--  - the type of the variable being colored (t)
-- output: a color
--  If a register color is available, return that one
--  Otherwise, if the variable type is a vector, return a root stack color
--  Otherwise, pick a regular stack color
--  HINT: use the `pickRootStackColor` and `pickStackColor` functions
pickColor :: Saturation -> Type -> Color
pickColor sat t =
  case pickRegColor sat of
    Just c -> undefined
    Nothing -> undefined

-- find the "smallest" root stack color not in a saturation
-- input:  saturation (sat) and lowest color to consider (c)
-- output: lowest root stack color not present in "sat"
pickRootStackColor :: Saturation -> Int -> Color
pickRootStackColor sat c | Set.member (RootStackColor c) sat = pickRootStackColor sat (c+1)
                         | otherwise = RootStackColor c

-- find the "smallest" stack color not in a saturation
-- input:  saturation (sat) and lowest color to consider (c)
-- output: lowest stack color not present in "sat"
pickStackColor :: Saturation -> Int -> Color
pickStackColor sat c | Set.member (StackColor c) sat = pickStackColor sat (c+1)
                     | otherwise = StackColor c

-- get the colors assigned to a list of variables in the coloring assignment
-- input:
--  - a coloring (cs)
--  - a list of variables
-- output: a set of colors, containing all the colors assigned to variables in the
--  list by the coloring "cs"
getColors :: Coloring -> [X86Arg] -> Set Color
getColors cs [] = Set.empty
getColors cs (x : xs) | Map.member x cs = Set.insert (cs Map.! x) (getColors cs xs)
                      | otherwise       = getColors cs xs

-- get the saturation of a node in the interference graph
-- input:
--  - a variable (x)
--  - a coloring (cs)
--  - an interference graph (g)
-- output: the saturation of x given cs and g
getSaturation :: TypedVariable -> Coloring -> Graph X86Arg -> Saturation
getSaturation (x, t) cs g =
  let ns = Set.toList (neighbors g (VarXE x t))
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
maxSat :: [TypedVariable] -> Coloring -> Graph X86Arg -> TypedVariable
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
colorGraph :: [TypedVariable] -> Graph X86Arg -> Coloring -> Coloring
colorGraph [] g cs = cs
colorGraph xs g cs = undefined

-- get the variables used in a program
-- input: a list of instructions
-- output: a list of variables
getLocalVars :: [X86Instr] -> [TypedVariable]
getLocalVars instrs = Set.toList (Set.unions (map varsInstr instrs))

varsInstr :: X86Instr -> Set TypedVariable
varsInstr e = case e of
  MovqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  AddqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  RetqE -> Set.empty
  CmpqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  JmpIfE _ _ -> Set.empty
  JmpE _ -> Set.empty
  MovzbqE (ByteRegE _) a2 -> varsArg a2
  SetE _ (ByteRegE _) -> Set.empty
  XorqE a1 a2 -> Set.union (varsArg a1) (varsArg a2)
  CallqE f -> Set.empty
  _ -> error (show e)

-- Given a register (reg) and an offset (i), build a stack location (as an X86Arg)
mkStackLoc :: Int -> String -> X86Arg
mkStackLoc i reg = DerefE reg (-8 * (i + 1))

-- given a coloring, a map from colors to their locations (on the stack or in a register),
-- and a variable, return the assigned location for that variable
-- input:
--  - a coloring (cs)
--  - a variable (x)
-- output: the location for the variable "x"
getHome :: Coloring -> TypedVariable -> (Variable, X86Arg)
getHome cs (x, t) =
  let color = cs Map.! (VarXE x t)
      loc   = case color of
                RegColor r -> undefined
                StackColor i -> undefined
                RootStackColor i -> undefined
  in (x, loc)

-- When printing out the homes and coloring information, skip printing
-- if the output would be longer than this value (default: 5000 characters).
-- To see the output when information is really long, modify this value
-- to a larger number.
longestOutputAllowed = 5000

-- Print a list in a nicely-formatted way (for homes and coloring)
showL :: (Show a) => [a] -> String
showL xs =
  let str = "[ " ++ intercalate "\n, " (map show xs) ++ " ]"
  in if (length str) > longestOutputAllowed
  then " **** LONG OUTPUT OMITTED, modify 'longestOutputAllowed' to see it ****"
  else str

-- Count the number of variables spilled to both stacks
-- Input: the list of colors assigned during allocation
-- Output: a pair
--  - the number of variables spilled to the stack
--  - the number of variables spilled to the root stack
countSpills :: [Color] -> (Int, Int)
countSpills [] = (0, 0)
countSpills (a : as) =
  let (stackSpills, rootStackSpills) = countSpills as
  in case a of
    RegColor _ -> (stackSpills, rootStackSpills)
    StackColor _ -> (stackSpills + 1, rootStackSpills)
    RootStackColor _ -> (stackSpills, rootStackSpills + 1)

-- the allocate-registers pass
-- input:  a pair, containing an interference graph and list of pseudo-x86 instructions
-- output: a pair, containing a list of x86 instructions and the number of stack locations used
allocateRegisters :: (Graph X86Arg, X86CFG) -> (X86CFG, (Int, Int))
allocateRegisters (g, cfg) =
  let locals       :: [TypedVariable]      = nub (concat (map (getLocalVars . snd) cfg))
      initColoring :: Coloring             = Map.fromList (zip registerArgs regColors)
      coloring     :: Coloring             = colorGraph locals g initColoring
      homes        :: [(Variable, X86Arg)] = map (getHome coloring) locals
      cfg'         :: X86CFG               = map (ahBlock homes) cfg
      spills       :: (Int, Int)           = countSpills (map snd (Map.toList coloring))
  in trace ("\nColoring:\n" ++ (showL (Map.toList coloring)) ++
            "\n\nHomes:\n" ++ (showL homes) )
  (cfg', spills)

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
patchInstructions :: (X86CFG, (Int, Int)) -> (X86CFG, (Int, Int))
patchInstructions (cfg, numSpills) = (map piBlock cfg, numSpills)

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
printX86Arg e = case e of
  VarXE s t -> "%%" ++ s
  RegE r -> "%" ++ r
  IntXE i -> "$" ++ (show i)
  DerefE r i -> (show i) ++ "(%" ++ r ++ ")"
  ByteRegE r -> "%" ++ r
  GlobalValXE s -> s ++ "(%rip)"

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
printX86 :: (X86CFG, (Int, Int)) -> String
printX86 (cfg, (stackSpills, rootStackSpills)) =
  let stackSize = align (8 * stackSpills) 16
      rootStackInit = concat (map (\_-> " movq $0, (%r15)\n addq $8, %r15\n") [0..rootStackSpills])
  in
  " .globl " ++ (printFun "main") ++ "\n" ++
  (printFun "main") ++ ":\n" ++
  " pushq %rbp\n" ++
  " movq %rsp, %rbp\n" ++
  " subq $" ++ (show stackSize) ++ ", %rsp\n" ++
  " movq $" ++ (show rootStackSize) ++ ", %rdi\n" ++
  " movq $" ++ (show heapSize) ++ ", %rsi\n" ++
  " callq " ++ (printFun "initialize") ++ "\n" ++
  " movq " ++ (printFun "rootstack_begin") ++ "(%rip), %r15\n" ++
  rootStackInit ++
  " jmp " ++ (printFun "start") ++ "\n" ++
  (intercalate "\n" $ map printX86Block (reverse cfg)) ++ "\n" ++
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

compile :: R3Expr -> String
compile = printX86 . patchInstructions . allocateRegisters . buildInterference .
  uncoverLive . selectInstructions . explicateControl . rcoExp . exposeAllocation .
  uniquify . shrink . typecheck

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

compileLog :: R3Expr -> IO String
compileLog e =
  (logOutput "input" id) e >>=
  (logOutput "typecheck" typecheck) >>=
  (logOutput "shrink" shrink) >>=
  (logOutput "uniquify" uniquify) >>=
  (logOutput "exposeAllocation" exposeAllocation) >>=
  (logOutput "rcoExp" rcoExp) >>=
  (logOutput "explicateControl" explicateControl) >>=
  (logOutput "selectInstructions" selectInstructions) >>=
  (logOutput "uncoverLive" uncoverLive) >>=
  (logOutput "buildInterference" buildInterference) >>=
  (logOutput "allocateRegisters" allocateRegisters) >>=
  (logOutput "patchInstructions" patchInstructions) >>=
  (logOutput "printX86" printX86)
