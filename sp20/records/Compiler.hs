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

argRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

-- size of the root stack and heap
rootStackSize = 2^14
heapSize = 2^4

------------------------------------------------------------
-- typecheck
------------------------------------------------------------

type TEnv = [(Variable, Type)]
type RecEnv = [(String, R5RecordDef)]

-- Typechecker for R5 Expressions
-- Inputs:
--  - e: an R5 expression
--  - env: a type environment mapping variables to their types
-- Output: a pair
--  - the type of e, if it is well-typed
--  - a *typed* R5 expression
-- Throws a run-time error if e is not well-typed
tcExpr :: R5Expr -> TEnv -> RecEnv -> (Type, TypedR5Expr)
tcExpr e env records = undefined

-- Get the name and type of a definition
getDefnType :: R5Definition -> (Variable, Type)
getDefnType (Defn name args outputType body) =
  (name, FunT (map snd args) outputType)

-- Typechecker for R5 definitions
tcDefn :: TEnv -> RecEnv -> R5Definition -> TypedR5Definition
tcDefn env records (Defn name args outputT body) =
  let env'           = args ++ env
      (bodyT, body') = tcExpr body env' records
  in case bodyT == outputT of
    True -> DefnT name args outputT body'
    False -> error ("Type error in definition: output type " ++ (show outputT) ++
                    " does not match body type " ++ (show bodyT))

mkRecConst :: R5RecordDef -> (Variable, Type)
mkRecConst (RecDef name fields) =
  let fieldNames = map fst fields
      fieldTypes = map snd fields
  in (name, FunT fieldTypes (RecordNameT name))

-- Typechecker for R5 programs
typecheck :: R5Program -> TypedR5Program
typecheck (records, defns, e) =
  let defTypeEnv = map getDefnType defns
      initEnv    = (map mkRecConst records) ++ defTypeEnv
      recordsEnv = map (\(RecDef name fields) -> (name, RecDef name fields)) records
      defns'     = map (tcDefn initEnv recordsEnv) defns
      (t, e')    = tcExpr e initEnv recordsEnv
  in trace ("Type: " ++ (show t)) (records, defns', e')


------------------------------------------------------------
-- shrink
------------------------------------------------------------

-- Shrink, for R5 expressions
-- Input: e, an R5 expression
-- Output: an R5 expression
-- Removes "and", "or", ">=", ">", ">="
shrinkExpr :: TypedR5Expr -> TypedR5Expr
shrinkExpr e = undefined

-- The shrink pass, for an R5 definition
-- Input:  an R5 definition
-- Output: an R5 definition
shrinkDefn :: TypedR5Definition -> TypedR5Definition
shrinkDefn (DefnT name args outputT body) =
  (DefnT name args outputT (shrinkExpr body))

-- The shrink pass, for an R5 program
-- Input:  an R5 program
-- Output: a list of R5 definitions
-- Moves the top-level expression into a "main" definition
shrink :: TypedR5Program -> ([R5RecordDef], [TypedR5Definition])
shrink (records, defns, e) =
  let defns'   = map shrinkDefn defns
      e'       = shrinkExpr e
      mainDefn = DefnT "main" [] IntT e'
  in (records, mainDefn : defns')

------------------------------------------------------------
-- uniquify
------------------------------------------------------------

-- A Variable Environment maps variables to variables
type VEnv = [(Variable, Variable)]

-- The uniquify pass, for an expression
-- Input:
--   - an R5 expression
--   - a variable environment
-- Output: an R5 expression, with variables renamed to be unique
uniquifyExp :: TypedR5Expr -> VEnv -> TypedR5Expr
uniquifyExp e env = undefined

-- The uniquify pass, for a single R5 definition
-- Input:  an R5 definition
-- Output: an R5 definition, with variables renamed to be unique
uniquifyDefn :: VEnv -> TypedR5Definition -> TypedR5Definition
uniquifyDefn initEnv (DefnT name args outputT body) =
  let argNames  = map fst args
      argTypes  = map snd args
      argNames' = map gensym argNames
      vEnv      = zip argNames argNames'
      body'     = uniquifyExp body (vEnv ++ initEnv)
  in DefnT name (zip argNames' argTypes) outputT body'

-- The uniquify pass, for a list of R5 definitions
-- Input:  a list of R5 definitions
-- Output: a list of R5 definitions, with variables renamed to be unique
uniquify :: ([R5RecordDef], [TypedR5Definition]) -> ([R5RecordDef], [TypedR5Definition])
uniquify (records, defns) =
  let initEnv  = map (\(DefnT name _ _ _) -> (name, name)) defns
      recEnv   = map (\(RecDef name _) -> (name, name)) records
  in (records, map (uniquifyDefn (initEnv ++ recEnv)) defns)

------------------------------------------------------------
-- convert-records
------------------------------------------------------------

crecExpr :: TypedR5Expr -> TypedR5Expr
crecExpr e = undefined

crecDefn :: TypedR5Definition -> TypedR5Definition
crecDefn (DefnT name args outputT body) =
  (DefnT name args outputT (crecExpr body))

crecRecord :: R5RecordDef -> TypedR5Definition
crecRecord (RecDef name fields) =
  let fieldNames = map fst fields
      fieldTypes = map snd fields
      fieldNameVars = map (\(x, t) -> VarTE x t) fields
      body = VectorTE fieldNameVars (VectorT fieldTypes)
  in DefnT name fields (RecordNameT name) body

convertRecords :: ([R5RecordDef], [TypedR5Definition]) -> [TypedR5Definition]
convertRecords (records, defns) = (map crecRecord records) ++ (map crecDefn defns)

------------------------------------------------------------
-- reveal-functions
------------------------------------------------------------

-- Reveal-functions, for R5 expressions
-- Input: e, an R5 expression
-- Output: an R5 expression
-- Transforms variables referencing function names with FunRefTE expressions
revealExpr :: TypedR5Expr -> [String] -> TypedR5Expr
revealExpr e funs = undefined

-- Reveal-functions, for R5 expressions
-- Input: e, an R5 expression
-- Output: an R5 expression
-- Transforms variables referencing function names with FunRefTE expressions
revealDefn :: [String] -> TypedR5Definition -> TypedR5Definition
revealDefn funs (DefnT name args outputT body) =
  (DefnT name args outputT (revealExpr body funs))

-- Reveal-functions, for a list of R5 definitions
-- Input: a list of R5 definitions
-- Output: a list of R5 definitions
-- Transforms variables referencing function names with FunRefTE expressions
revealFunctions :: [TypedR5Definition] -> [TypedR5Definition]
revealFunctions defns =
  let funs = map (\(DefnT name _ _ _) -> name) defns
  in map (revealDefn funs) defns


------------------------------------------------------------
-- closure-conversion
------------------------------------------------------------

-- Free variables of an expression
freeVars :: TypedR5Expr -> Set TypedVariable
freeVars e = undefined

-- Closure conversion, for R5 expressions
-- Input: e, an R5 expression
-- Output: a pair
--  - an R5 expression
--  - a list of new top-level definitions (from closure conversion)
cconvExpr :: TypedR5Expr -> (TypedR5Expr, [TypedR5Definition])
cconvExpr e = undefined

-- Build a new definition for a closure being converted
mkClosureDefn :: String -> [TypedVariable] -> [TypedVariable] ->
  TypedR5Expr -> Type -> TypedR5Definition
mkClosureDefn name fvs args body outT =
  let closName = gensym "clos"
      fvTs     = map snd fvs
      closT    = VectorT (UnknownT : fvTs)
      closVar  = VarTE closName closT
      bs       = map (\((x,t),i) -> (x, VectorRefTE closVar i UnknownT)) (zip fvs [1..])
      body'    = mkLet bs body
      allArgs  = (closName, closT) : args
  in DefnT name allArgs outT body'

-- Closure conversion, for R5 definitions
-- Input: an R5 definition
-- Output: an R5 definition
cconvDefn :: TypedR5Definition -> [TypedR5Definition]
cconvDefn (DefnT name args outputT body) =
  let (body', cs) = cconvExpr body
      closT       = VectorT [UnknownT]
      closArg     = (gensym "clos", closT)
      args'       = if name == "main" then args else closArg : args
  in (DefnT name args' outputT body') : cs

-- Closure conversion, for a list of R5 definitions
-- Input: a list of R5 definitions
-- Output: a list of R5 definitions
closureConversion :: [TypedR5Definition] -> [TypedR5Definition]
closureConversion defns = concat (map cconvDefn defns)

------------------------------------------------------------
-- limit-functions
------------------------------------------------------------

-- An environment mapping variables to expressions
-- For replacing variables with vectorRef expressions in functions with >6 arguments
type EEnv = [(Variable, TypedR5Expr)]

-- Limit-functions, for R5 expressions
-- Input:
--  - e, an R5 expression
--  - env, an environment
-- Output: an R5 expression
-- Moves arguments > 6 into a vector
limitExpr :: TypedR5Expr -> EEnv -> TypedR5Expr
limitExpr e env = undefined

-- Limit-functions, for an R5 definition
-- Input: an R5 definition
-- Output: an R5 definition
-- Moves arguments > 6 into a vector
limitDefn :: TypedR5Definition -> TypedR5Definition
limitDefn (DefnT name args outputT body) =
  case (length args) > 6 of
    True -> let vecArgName = gensym "vecArg"
                vecArgs = drop 5 args
                vecArgTypes = map snd vecArgs
                vecArgType = VectorT vecArgTypes
                vecArgExpr = VarTE vecArgName vecArgType
                newArgs = (take 5 args) ++ [(vecArgName, vecArgType)]
                env = map (\((var, t), i) -> (var, VectorRefTE vecArgExpr i t)) (zip vecArgs [0..])
            in DefnT name newArgs outputT (limitExpr body env)
    False -> DefnT name args outputT (limitExpr body []) -- no change needed

-- Limit-functions, for a list of R5 definitions
-- Input: a list of R5 definitions
-- Output: a list of R5 definitions
-- Moves arguments > 6 into a vector
limitFunctions :: [TypedR5Definition] -> [TypedR5Definition]
limitFunctions defns = map limitDefn defns

------------------------------------------------------------
-- expose-allocation
------------------------------------------------------------

-- a Binding maps a variable to a typed expression
type Binding = (Variable, TypedR5Expr)

-- Make a "LET" expression from a list of bindings and a final "body" expression
mkLet :: [Binding] -> TypedR5Expr -> TypedR5Expr
mkLet [] body = body
mkLet ((x, e) : bs) body = LetTE x e (mkLet bs body)

-- Expose allocation, for an R5 expression
-- Input: an R5 expression
-- Output: an R5 expression, without "VectorTE" expressions
-- This pass compiles "VectorTE" expressions into explicit allocations
eaExp :: TypedR5Expr -> TypedR5Expr
eaExp e = undefined

-- Expose allocation, for an R5 definition
-- Input: an R5 definition
-- Output: an R5 definition, without "VectorTE" expressions
eaDefn :: TypedR5Definition -> TypedR5Definition
eaDefn (DefnT name args outputT body) =
  DefnT name args outputT (eaExp body)

-- Expose allocation, for an R5 expression
-- Input: an R5 expression
-- Output: an R5 expression, without "VectorTE" expressions
exposeAllocation :: [TypedR5Definition] -> [TypedR5Definition]
exposeAllocation defns = map eaDefn defns

------------------------------------------------------------
-- remove-complex-opera
------------------------------------------------------------

-- The remove-complex-operand pass on an expression in TAIL POSITION
-- input:  COMPLEX EXPRESSION
-- output: COMPLEX EXPRESSION in A-Normal Form
rcoExp :: TypedR5Expr -> TypedR5Expr
rcoExp e = undefined

-- The remove-complex-operand pass on an expression in ARGUMENT POSITION
-- input:  COMPLEX EXPRESSION
-- output: pair: SIMPLE EXPRESSION and LIST OF BINDINGS
-- 
-- the LIST OF BINDINGS maps variables to SIMPLE EXPRESSIONS
rcoArg :: TypedR5Expr -> (TypedR5Expr, [Binding])
rcoArg e = undefined

-- Remove complex operands, for an R5 definition
-- Input: an R5 definition
-- Output: an R5 definition, without complex operands
rcoDefn :: TypedR5Definition -> TypedR5Definition
rcoDefn (DefnT name args outputT body) =
  DefnT name args outputT (rcoExp body)

-- Remove complex operands, for an R5 program
-- Input: an R5 program
-- Output: an R5 program, without complex operands
removeComplexOpera :: [TypedR5Definition] -> [TypedR5Definition]
removeComplexOpera defns = map rcoDefn defns

------------------------------------------------------------
-- explicate-control
------------------------------------------------------------
type Label = String

data C3Arg = IntC3 Int
           | VarC3 Variable Type
           | TrueC3
           | FalseC3
           | VoidC3
           | GlobalValC3 String
  deriving (Eq, Ord, Show)

data C3Cmp = CmpEqC3
           | CmpLTC3
  deriving (Eq, Ord, Show)

data C3Basic = C3ArgE C3Arg
             | C3PlusE C3Arg C3Arg
             | C3NotE C3Arg
             | C3CmpE C3Cmp C3Arg C3Arg
             | C3AllocateE Int Type
             | C3VectorRefE C3Arg Int
             | C3VectorSetE C3Arg Int C3Arg
             | C3FunRefE Label Type
             | C3CallE C3Arg [C3Arg]
  deriving (Eq, Ord, Show)

data C3Stmt = AssignC3 Variable Type C3Basic
            | CollectC3 Int
  deriving (Eq, Ord, Show)

data C3Tail = ReturnC3 C3Basic
            | SeqC3 C3Stmt C3Tail
            | GotoC3 Label
            | IfC3 C3Cmp C3Arg C3Arg Label Label
            | TailCallC3 C3Arg [C3Arg]
  deriving (Eq, Ord, Show)

type C3CFG = CFG C3Tail

data C3Defn = DefnC3 Label [(Variable, Type)] Type C3CFG
  deriving (Eq, Show)

-- Compile a R5 argument (integer or variable) into a C3Arg expression
ecArg :: TypedR5Expr -> C3Arg
ecArg e = case e of
  IntTE i -> IntC3 i
  VarTE x t -> VarC3 x t
  TrueTE -> TrueC3
  FalseTE -> FalseC3
  VoidTE -> VoidC3
  GlobalValTE s -> GlobalValC3 s
  _ -> error $ show e

ecCmp :: Cmp -> C3Cmp
ecCmp c = case c of
  CmpEqual -> CmpEqC3
  CmpLT -> CmpLTC3

-- Compile a BASIC R5 Expression into a C3Basic Expression
ecBasic :: TypedR5Expr -> C3Basic
ecBasic e = undefined

-- The explicate-control pass on an expression in TAIL POSITION
-- inputs:
--  - e: a COMPLEX EXPRESSION in A-Normal Form
--  - cfg: a C3CFG (control flow graph)
-- output: a C3 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecTail :: TypedR5Expr -> C3CFG -> C3Tail
ecTail e cfg = undefined

-- The explicate-control pass on an expression in PREDICATE POSITION
-- inputs:
--  - test: a COMPLEX R5 EXPRESSION
--  - b1: a C3 Tail expression (the "then-block")
--  - b2: a C3 Tail expression (the "else-block")
--  - cfg: a C3CFG (control flow graph)
-- output: a C3 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecPred :: TypedR5Expr -> C3Tail -> C3Tail -> C3CFG -> C3Tail
ecPred test b1 b2 cfg = undefined

-- The explicate-control pass on an expression in ASSIGNMENT POSITION
-- input:
--   - x: the variable being assigned
--   - e: the R5 Expression it is being assigned to
--   - k: a C3 Tail expression describing what should happen *after* the assignment
--   - cfg: a C3CFG (control flow graph)
-- output: a C3 Tail expression
-- Sometimes updates 'cfg' with new CFG nodes
ecAssign :: Variable -> TypedR5Expr -> C3Tail -> C3CFG -> C3Tail
ecAssign x e k cfg = undefined

ecDefn :: TypedR5Definition -> C3Defn
ecDefn (DefnT name args outputT body) =
  let cfg = emptyCFG ()
      b = case name of
        "main" -> let tmp = gensym "result"
                  in ecAssign tmp body (ReturnC3 (C3ArgE (VarC3 tmp IntT))) cfg
        _ -> ecTail body cfg
      _   = addCFGNode cfg name b
  in DefnC3 name args outputT cfg

-- The explicate control pass
-- input: an R5 expression
-- output: a C3 control flow graph
explicateControl :: [TypedR5Definition] -> [C3Defn]
explicateControl defns = map ecDefn defns


------------------------------------------------------------
-- select-instructions
------------------------------------------------------------

data X86Arg = VarXE Variable Type
            | DerefE String Int
            | RegE String
            | ByteRegE String
            | IntXE Int
            | GlobalValXE String
            | FunRefXE Label
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
              | IndirectCallqE X86Arg
              | TailJmpE X86Arg
              | LeaqE X86Arg X86Arg
  deriving (Eq, Ord, Show)

type X86CFG = [(Label, [X86Instr])]

data X86Definition = X86Def Label [(Variable, Type)] Type X86CFG
  deriving (Eq, Ord, Show)

-- select instructions for a C3 Arg
-- returns an x86 argument
siArg :: C3Arg -> X86Arg
siArg e = case e of
  IntC3 i -> IntXE i
  VarC3 x t -> VarXE x t
  TrueC3 -> IntXE 1
  FalseC3 -> IntXE 0
  VoidC3 -> IntXE 0
  GlobalValC3 s -> GlobalValXE s

-- select instructions for a C3 comparison
-- returns an x86 comparison code
siCC :: C3Cmp -> X86CC
siCC c = case c of
  CmpEqC3 -> CCe
  CmpLTC3 -> CCl

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
mkTag ts =
  let len = length ts
      pMask = mkPointerMask ts
      pMaskAndLength = (Bits.shift pMask 6) + len
  in (Bits.shift pMaskAndLength 1) + 1

-- The select-instructions pass on a C3Stmt statement
-- input:  a C3Stmt
-- output: a list of pseudo-x86 instructions
siStmt :: C3Stmt -> [X86Instr]
siStmt e = undefined

-- The select-instructions pass on a C3Tail expression
-- input:  a C3 Tail expression
-- output: a list of pseudo-X86 instructions
siTail :: C3Tail -> [X86Instr]
siTail e = undefined

-- The select-instructions pass, for a basic block
-- Input: a pair
--  - the basic block's label
--  - the basic block's code, as a C3 Tail
-- Output: a pair
--  - the basic block's label
--  - the basic block's code, as a list of pseudo-x86 instructions
siBlock :: (Label, C3Tail) -> (Label, [X86Instr])
siBlock (label, block) = (label, siTail block)

-- registers used for passing inputs to a function
callingRegs = map RegE ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

-- load an argument from an input register into a function-local variable
loadArg :: ((Variable, Type), X86Arg) -> X86Instr
loadArg ((var, typ), reg) = MovqE reg (VarXE var typ)

-- The select-instructions pass, for a definition
-- Input: a C3 definition
-- Output: an x86 definition
siDefn :: C3Defn -> X86Definition
siDefn (DefnC3 name args outputT cfg) =
  let argInstrs       = map loadArg (zip args callingRegs)
      x86cfg          = map siBlock (toListCFG cfg)
      mainBlockInstrs = fromJust (lookup name x86cfg)
      mainBlock'      = (name ++ "_start", argInstrs ++ mainBlockInstrs)
  in X86Def name args outputT (mainBlock' : (delete (name, mainBlockInstrs) x86cfg))

-- The select-instructions pass
-- Input: a C3 control flow graph
-- Output: an x86 control flow graph
selectInstructions :: [C3Defn] -> [X86Definition]
selectInstructions defns = map siDefn defns

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
  FunRefXE _ -> Set.empty
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
uncoverLive :: [X86Definition] -> [(LivenessResult, X86Definition)]
uncoverLive defs = map uncoverLiveDef defs

uncoverLiveDef :: X86Definition -> (LivenessResult, X86Definition)
uncoverLiveDef (X86Def name args outputT cfg) =
  let adjacentBlockSets = map adjacentBlocks cfg
      sortedLabels = tsort adjacentBlockSets
      sortedCFG = map (\l -> (l, fromJust (lookup l adjacentBlockSets))) sortedLabels
      initLiveBefores = map (\l -> (l, Set.empty)) sortedLabels
      lAfters = ulBlocks sortedCFG cfg initLiveBefores
  in (lAfters, X86Def name args outputT cfg)

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
  in addEdge g' d (VarXE v UnknownT)

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
buildInterference :: [(LivenessResult, X86Definition)] -> [(Graph X86Arg, X86Definition)]
buildInterference ps = map biDef ps

biDef :: (LivenessResult, X86Definition) -> (Graph X86Arg, X86Definition)
biDef (liveness, X86Def name args outputT cfg) =
  let labels = map fst cfg
      g = biBlocks labels cfg liveness emptyGraph
  in (g, X86Def name args outputT cfg)


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

-- A "coloring" for a graph is a mapping from variables in the graph to colors
type Coloring = Map Variable Color

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
pickColor :: Saturation -> Type -> Color
pickColor sat t = undefined

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
getColors cs (RegE r : xs) = Set.insert (RegColor r) (getColors cs xs)
getColors cs (VarXE x t : xs) | Map.member x cs = Set.insert (cs Map.! x) (getColors cs xs)
                              | otherwise       = getColors cs xs

-- get the saturation of a node in the interference graph
-- input:
--  - a variable (x)
--  - a coloring (cs)
--  - an interference graph (g)
-- output: the saturation of x given cs and g
getSaturation :: TypedVariable -> Coloring -> Graph X86Arg -> Saturation
getSaturation (x, t) cs g =
  let ns = Set.toList (neighbors g (VarXE x UnknownT))
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
colorGraph xs g cs =
  let (x, t) = maxSat xs cs g                          -- find maximally saturated variable
      xs'    = delete (x, t) xs                        -- remove it from the list
      color  = pickColor (getSaturation (x, t) cs g) t -- pick a color for it
      newCs  = Map.insert x color cs                   -- record its coloring
  in colorGraph xs' g newCs

-- get the variables used in a program
-- input: a list of instructions
-- output: a list of variables
getLocalVars :: [X86Instr] -> [TypedVariable]
getLocalVars instrs = Set.toList (Set.unions (map varsInstr instrs))

varsInstr :: X86Instr -> Set TypedVariable
varsInstr e = undefined

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
getHome cs (x, t) = undefined

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
allocateRegisters :: [(Graph X86Arg, X86Definition)] -> [(X86Definition, (Int, Int))]
allocateRegisters ps = map arDef ps

-- the allocate-registers pass, for a definition
-- input:
--  - g: the interference graph for the definition
--  - the definition itself
-- output: a pair, containing a definition and the number of variables spilled
--  to the stack and root stack
arDef :: (Graph X86Arg, X86Definition) -> (X86Definition, (Int, Int))
arDef (g, X86Def name args outputT cfg) =
  let locals    :: [TypedVariable]      = nub (concat (map (getLocalVars . snd) cfg))
      initColoring :: Coloring          = Map.empty
      coloring  :: Coloring             = colorGraph locals g initColoring
      homes :: [(Variable, X86Arg)]     = map (getHome coloring) locals
      cfg'      :: X86CFG               = map (ahBlock homes) cfg
      spills                            = countSpills (map snd (Map.toList coloring))
  in trace ("\nColoring:\n" ++ (showL (Map.toList coloring)) ++
            "\n\nHomes:\n" ++ (showL homes) )
  (X86Def name args outputT cfg', spills)

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
patchInstructions :: [(X86Definition, (Int, Int))] -> [(X86Definition, (Int, Int))]
patchInstructions ps = map piDef ps

piDef :: (X86Definition, (Int, Int)) -> (X86Definition, (Int, Int))
piDef (X86Def name args outputT cfg, numSpills) =
  (X86Def name args outputT (map piBlock cfg), numSpills)

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
  GlobalValXE s -> (printFun s) ++ "(%rip)"
  FunRefXE f -> (printFun f) ++ "(%rip)"
  _ -> error $ show e

-- The printX86 pass for x86 instructions
printX86Instr :: (Int, Int) -> Label -> X86Instr -> String
printX86Instr (stackSpills, rootStackSpills) name e = undefined

-- The printX86 pass for a single block
-- Input:
--   - spills: the number of variables spilled to the regular and root stacks
--   - name: the name of the function being printed
--   - (l, instrs): the label and instructions of the block to print
-- Output: x86 assembly, as a string
printX86Block :: (Int, Int) -> Label -> (Label, [X86Instr]) -> String
printX86Block spills name (l, instrs) =
  (printFun l) ++ ":\n" ++
  (intercalate "\n" $ map (printX86Instr spills name) instrs)


-- The printX86 pass for multiple block
-- Input:
--   - spills: the number of variables spilled to the regular and root stacks
--   - name: the name of the function being printed
--   - cfg: the control flow graph for the function being printed
-- Output: x86 assembly, as a string
printX86Blocks :: (Int, Int) -> Label -> [(Label, [X86Instr])] -> String
printX86Blocks spills name cfg =
  (intercalate "\n" $ map (printX86Block spills name) cfg) ++ "\n"

-- The printX86 pass for x86 programs
-- Input: a pair
--   - a list of instructions
--   - the number of stack locations used in the program
-- Output: x86 assembly, as a string
printX86 :: [(X86Definition, (Int, Int))] -> String
printX86 ps =
  " .globl " ++ (printFun "main") ++ "\n" ++
  (concat (map printX86Def ps))


-- Initialize the root stack
-- Input:
--  - name: the name of the function being printed
--  - rootStackSpills: the number of variables spilled to the root stack
-- Output: a string containing instructions for initializing the root stack
rootStackInit :: String -> Int -> String
rootStackInit name rootStackSpills =
  -- for each spilled variable, initialize the stack location for it
  let initInstrs = concat (take rootStackSpills (repeat " movq $0, (%r15)\n addq $8, %r15\n"))
  in case name of
       "main" ->
         -- if we're in main, initialize the root stack itself
         " movq $" ++ (show rootStackSize) ++ ", %rdi\n" ++
         " movq $" ++ (show heapSize) ++ ", %rsi\n" ++
         " callq " ++ (printFun "initialize") ++ "\n" ++
         " movq " ++ (printFun "rootstack_begin") ++ "(%rip), %r15\n" ++
         -- then initialize the root stack locations we'll use
         initInstrs
       _ -> initInstrs  -- otherwise just initialize the root stack locations


printX86Def :: (X86Definition, (Int, Int)) -> String
printX86Def (X86Def name args outputT cfg, (stackSpills, rootStackSpills)) =
  let stackSize = align (8 * stackSpills) 16
      rootStackSize = 8 * rootStackSpills
      pushqs = map (\r -> " pushq %" ++ r ++ "\n") calleeSavedRegisters
      popqs = map (\r -> " popq %" ++ r ++ "\n") (reverse calleeSavedRegisters)
  in
  -- entry point for the function
  (printFun (name)) ++ ":\n" ++
  -- set up the stack
  " pushq %rbp\n" ++
  " movq %rsp, %rbp\n" ++
  -- save callee-saved registers
  (concat pushqs) ++
  -- add space on the stack for stack variables
  " subq $" ++ (show stackSize) ++ ", %rsp\n" ++
  -- initialize the root stack
  (rootStackInit name rootStackSpills) ++
  -- jump to the function body
  " jmp " ++ (printFun (name ++ "_start")) ++ "\n" ++
  -- print the labels and instructions for the blocks of this function
  (printX86Blocks (stackSpills, rootStackSpills) name (reverse cfg)) ++
  -- print the function conclusion
  (printFun (name ++ "_conclusion")) ++ ":\n" ++
  -- if we're in main, call print_int
  (case name of
     "main" ->
       " movq %rax, %rdi\n" ++
       " callq " ++ (printFun "print_int") ++ "\n" ++
       " movq $0, %rax\n"
     _ -> "") ++
  -- shrink the root stack
  " subq $" ++ (show rootStackSize) ++ ", %r15\n" ++
  -- shrink the stack
  " addq $" ++ (show stackSize) ++ ", %rsp\n" ++
  -- restore callee-saved registers
  (concat popqs) ++
  -- restore stack pointer
  " popq %rbp\n" ++
  -- return
  " retq\n"

------------------------------------------------------------
-- compile / main
------------------------------------------------------------

compile :: R5Program -> String
compile = printX86 . patchInstructions . allocateRegisters . buildInterference .
  uncoverLive . selectInstructions . explicateControl . removeComplexOpera . exposeAllocation .
  limitFunctions . closureConversion . revealFunctions . convertRecords . uniquify .
  shrink . typecheck

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

compileLog :: R5Program -> IO String
compileLog e =
  (logOutput "input" id) e >>=
  (logOutput "typecheck" typecheck) >>=
  (logOutput "shrink" shrink) >>=
  (logOutput "uniquify" uniquify) >>=
  (logOutput "convertRecords" convertRecords) >>=
  (logOutput "revealFunctions" revealFunctions) >>=
  (logOutput "closureConversion" closureConversion) >>=
  (logOutput "limitFunctions" limitFunctions) >>=
  (logOutput "exposeAllocation" exposeAllocation) >>=
  (logOutput "removeComplexOpera" removeComplexOpera) >>=
  (logOutput "explicateControl" explicateControl) >>=
  (logOutput "selectInstructions" selectInstructions) >>=
  (logOutput "uncoverLive" uncoverLive) >>=
  (logOutput "buildInterference" buildInterference) >>=
  (logOutput "allocateRegisters" allocateRegisters) >>=
  (logOutput "patchInstructions" patchInstructions) >>=
  (logOutput "printX86" printX86)
