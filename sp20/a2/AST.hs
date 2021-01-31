module AST where

data R1Expr = IntE Int
            | VarE String
            | PlusE R1Expr R1Expr
            | LetE String R1Expr R1Expr
  deriving (Eq, Ord, Show)
