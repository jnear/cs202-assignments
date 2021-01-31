module AST where

data Cmp = CmpEqual
         | CmpGT
         | CmpLT
         | CmpGTE
         | CmpLTE
  deriving (Eq, Ord, Show)

data R2Expr = IntE Int
            | VarE String
            | PlusE R2Expr R2Expr
            | LetE String R2Expr R2Expr
            | TrueE
            | FalseE
            | AndE R2Expr R2Expr
            | OrE R2Expr R2Expr
            | NotE R2Expr
            | CmpE Cmp R2Expr R2Expr
            | IfE R2Expr R2Expr R2Expr
  deriving (Eq, Ord, Show)
