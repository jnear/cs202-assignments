module AST where

data R0Expr = IntE Integer
            | PlusE R0Expr R0Expr
  deriving (Eq, Ord, Show)
