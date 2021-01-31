module AST where

data Cmp = CmpEqual
         | CmpGT
         | CmpLT
         | CmpGTE
         | CmpLTE
  deriving (Eq, Ord, Show)

data R3Expr = IntE Int
            | VarE String
            | PlusE R3Expr R3Expr
            | LetE String R3Expr R3Expr
            | TrueE
            | FalseE
            | AndE R3Expr R3Expr
            | OrE R3Expr R3Expr
            | NotE R3Expr
            | CmpE Cmp R3Expr R3Expr
            | IfE R3Expr R3Expr R3Expr
            | VoidE
            | VectorSetE R3Expr Int R3Expr
            | VectorRefE R3Expr Int
            | VectorE [R3Expr]
            | CollectE Int
            | AllocateE Int [String]
            | GlobalValE String
  deriving (Eq, Ord, Show)

data Type = IntT
          | BoolT
          | VectorT [Type]
          | VoidT
  deriving (Eq, Ord, Show)

data TypedR3Expr = IntTE Int
                 | VarTE String Type
                 | PlusTE TypedR3Expr TypedR3Expr
                 | LetTE String TypedR3Expr TypedR3Expr
                 | TrueTE
                 | FalseTE
                 | AndTE TypedR3Expr TypedR3Expr
                 | OrTE TypedR3Expr TypedR3Expr
                 | NotTE TypedR3Expr
                 | CmpTE Cmp TypedR3Expr TypedR3Expr
                 | IfTE TypedR3Expr TypedR3Expr TypedR3Expr Type
                 | VoidTE
                 | VectorSetTE TypedR3Expr Int TypedR3Expr
                 | VectorRefTE TypedR3Expr Int Type
                 | VectorTE [TypedR3Expr] Type
                 | CollectTE Int
                 | AllocateTE Int Type
                 | GlobalValTE String
  deriving (Eq, Ord, Show)
