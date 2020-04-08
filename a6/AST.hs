module AST where

-- An R4 Program is a list of definitions and a "main" expression
type R4Program = ([R4Definition], R4Expr)
type TypedR4Program = ([TypedR4Definition], TypedR4Expr)

-- Representation for R4 function definitions
-- Definitions have:
--  - a name
--  - a list of arguments (with name and type)
--  - an output type
--  - an R4 expression for the body
data R4Definition = Defn String [(String, Type)] Type R4Expr
  deriving (Eq, Ord, Show)

data TypedR4Definition = DefnT String [(String, Type)] Type TypedR4Expr
  deriving (Eq, Ord, Show)

data Cmp = CmpEqual
         | CmpGT
         | CmpLT
         | CmpGTE
         | CmpLTE
  deriving (Eq, Ord, Show)

data R4Expr = IntE Int
            | VarE String
            | PlusE R4Expr R4Expr
            | LetE String R4Expr R4Expr
            | TrueE
            | FalseE
            | AndE R4Expr R4Expr
            | OrE R4Expr R4Expr
            | NotE R4Expr
            | CmpE Cmp R4Expr R4Expr
            | IfE R4Expr R4Expr R4Expr
            | VoidE
            | VectorSetE R4Expr Int R4Expr
            | VectorRefE R4Expr Int
            | VectorE [R4Expr]
            | CollectE Int
            | AllocateE Int [String]
            | GlobalValE String
            | FunCallE R4Expr [R4Expr]
  deriving (Eq, Ord, Show)

data Type = IntT
          | BoolT
          | VectorT [Type]
          | VoidT
          | FunT [Type] Type
  deriving (Eq, Ord, Show)

data TypedR4Expr = IntTE Int
                 | VarTE String Type
                 | PlusTE TypedR4Expr TypedR4Expr
                 | LetTE String TypedR4Expr TypedR4Expr
                 | TrueTE
                 | FalseTE
                 | AndTE TypedR4Expr TypedR4Expr
                 | OrTE TypedR4Expr TypedR4Expr
                 | NotTE TypedR4Expr
                 | CmpTE Cmp TypedR4Expr TypedR4Expr
                 | IfTE TypedR4Expr TypedR4Expr TypedR4Expr Type
                 | VoidTE
                 | VectorSetTE TypedR4Expr Int TypedR4Expr
                 | VectorRefTE TypedR4Expr Int Type
                 | VectorTE [TypedR4Expr] Type
                 | CollectTE Int
                 | AllocateTE Int Type
                 | GlobalValTE String
                 | FunCallTE TypedR4Expr [TypedR4Expr] [Type] Type
                 | FunRefTE String Type
  deriving (Eq, Ord, Show)
