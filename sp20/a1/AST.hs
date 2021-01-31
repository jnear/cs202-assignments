module AST where

data R0Expr = IntE Int
            | PlusE R0Expr R0Expr
  deriving (Eq, Ord, Show)
