module AST where

-- An R5 Program is a list of definitions and a "main" expression
type R5Program = ([R5ClassDef], [R5Definition], R5Expr)
type TypedR5Program = ([R5ClassDef], [TypedR5Definition], TypedR5Expr)

-- Representation for R5 function definitions
-- Definitions have:
--  - a name
--  - a list of arguments (with name and type)
--  - an output type
--  - an R5 expression for the body
data R5Definition = Defn String [(String, Type)] Type R5Expr
  deriving (Eq, Ord, Show)

data TypedR5Definition = DefnT String [(String, Type)] Type TypedR5Expr
  deriving (Eq, Ord, Show)

-- Class type definition
data R5ClassDef = ClassDef String String [(String, Type)] [R5Definition]
  deriving (Eq, Ord, Show)

data TypedR5ClassDef = ClassDefT String String [(String, Type)] [TypedR5Definition]
  deriving (Eq, Ord, Show)

data Cmp = CmpEqual
         | CmpGT
         | CmpLT
         | CmpGTE
         | CmpLTE
  deriving (Eq, Ord, Show)

data R5Expr = IntE Int
            | VarE String
            | PlusE R5Expr R5Expr
            | LetE String R5Expr R5Expr
            | TrueE
            | FalseE
            | AndE R5Expr R5Expr
            | OrE R5Expr R5Expr
            | NotE R5Expr
            | CmpE Cmp R5Expr R5Expr
            | IfE R5Expr R5Expr R5Expr
            | VoidE
            | VectorSetE R5Expr Int R5Expr
            | VectorRefE R5Expr Int
            | VectorE [R5Expr]
            | CollectE Int
            | AllocateE Int [String]
            | GlobalValE String
            | FunCallE R5Expr [R5Expr]
            | LambdaE [(String, Type)] R5Expr
            | GetFieldE R5Expr String
  deriving (Eq, Ord, Show)

data Type = IntT
          | BoolT
          | VectorT [Type]
          | VoidT
          | FunT [Type] Type
          | ClassNameT String
          | ClassT String [(String, Type)]
          | UnknownT
  deriving (Eq, Ord, Show)

data TypedR5Expr = IntTE Int
                 | VarTE String Type
                 | PlusTE TypedR5Expr TypedR5Expr
                 | LetTE String TypedR5Expr TypedR5Expr
                 | TrueTE
                 | FalseTE
                 | AndTE TypedR5Expr TypedR5Expr
                 | OrTE TypedR5Expr TypedR5Expr
                 | NotTE TypedR5Expr
                 | CmpTE Cmp TypedR5Expr TypedR5Expr
                 | IfTE TypedR5Expr TypedR5Expr TypedR5Expr Type
                 | VoidTE
                 | VectorSetTE TypedR5Expr Int TypedR5Expr
                 | VectorRefTE TypedR5Expr Int Type
                 | VectorTE [TypedR5Expr] Type
                 | CollectTE Int
                 | AllocateTE Int Type
                 | GlobalValTE String
                 | FunCallTE TypedR5Expr [TypedR5Expr] [Type] Type
                 | FunRefTE String Type
                 | LambdaTE [(String, Type)] Type TypedR5Expr
                 | GetFieldTE TypedR5Expr String Int Type
  deriving (Eq, Ord, Show)
