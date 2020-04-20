module AST where

-- An R7 Program is a list of definitions and a "main" expression
type R7Program = ([R7Definition], R7Expr)

-- An R6 Program is a list of definitions and a "main" expression
type R6Program = ([R6Definition], R6Expr)
type TypedR6Program = ([TypedR6Definition], TypedR6Expr)

-- Representation for R6 function definitions
-- Definitions have:
--  - a name
--  - a list of arguments (with name and type)
--  - an output type
--  - an R6 expression for the body
data R6Definition = Defn String [(String, Type)] Type R6Expr
  deriving (Eq, Ord, Show)

data TypedR6Definition = DefnT String [(String, Type)] Type TypedR6Expr
  deriving (Eq, Ord, Show)

data R7Definition = DefnD String [String] R7Expr
  deriving (Eq, Ord, Show)

data Cmp = CmpEqual
         | CmpGT
         | CmpLT
         | CmpGTE
         | CmpLTE
  deriving (Eq, Ord, Show)

data R7Expr = IntDE Int
            | VarDE String
            | PlusDE R7Expr R7Expr
            | LetDE String R7Expr R7Expr
            | TrueDE
            | FalseDE
            | AndDE R7Expr R7Expr
            | OrDE R7Expr R7Expr
            | NotDE R7Expr
            | CmpDE Cmp R7Expr R7Expr
            | IfDE R7Expr R7Expr R7Expr
            | VoidDE
            | VectorSetDE R7Expr Int R7Expr
            | VectorRefDE R7Expr Int
            | VectorDE [R7Expr]
            | CollectDE Int
            | AllocateDE Int [String]
            | GlobalValDE String
            | FunCallDE R7Expr [R7Expr]
            | LambdaDE [String] R7Expr
  deriving (Eq, Ord, Show)

data R6Expr = IntE Int
            | VarE String
            | PlusE R6Expr R6Expr
            | LetE String R6Expr R6Expr
            | TrueE
            | FalseE
            | AndE R6Expr R6Expr
            | OrE R6Expr R6Expr
            | NotE R6Expr
            | CmpE Cmp R6Expr R6Expr
            | IfE R6Expr R6Expr R6Expr
            | VoidE
            | VectorSetE R6Expr Int R6Expr
            | VectorRefE R6Expr Int
            | VectorE [R6Expr]
            | CollectE Int
            | AllocateE Int [String]
            | GlobalValE String
            | FunCallE R6Expr [R6Expr]
            | LambdaE [(String, Type)] R6Expr
            | InjectE R6Expr Type
            | ProjectE R6Expr Type
  deriving (Eq, Ord, Show)

data Type = IntT
          | BoolT
          | VectorT [Type]
          | VoidT
          | FunT [Type] Type
          | UnknownT
          | AnyT
  deriving (Eq, Ord, Show)

data TypedR6Expr = IntTE Int
                 | VarTE String Type
                 | PlusTE TypedR6Expr TypedR6Expr
                 | LetTE String TypedR6Expr TypedR6Expr
                 | TrueTE
                 | FalseTE
                 | AndTE TypedR6Expr TypedR6Expr
                 | OrTE TypedR6Expr TypedR6Expr
                 | NotTE TypedR6Expr
                 | CmpTE Cmp TypedR6Expr TypedR6Expr
                 | IfTE TypedR6Expr TypedR6Expr TypedR6Expr Type
                 | VoidTE
                 | VectorSetTE TypedR6Expr Int TypedR6Expr
                 | VectorRefTE TypedR6Expr Int Type
                 | VectorTE [TypedR6Expr] Type
                 | CollectTE Int
                 | AllocateTE Int Type
                 | GlobalValTE String
                 | FunCallTE TypedR6Expr [TypedR6Expr] [Type] Type
                 | FunRefTE String Type
                 | LambdaTE [(String, Type)] Type TypedR6Expr
                 | InjectTE TypedR6Expr Type
                 | ProjectTE TypedR6Expr Type
                 | TagOfAnyTE TypedR6Expr
                 | ValueOfAnyTE TypedR6Expr Type
                 | ExitTE
  deriving (Eq, Ord, Show)
