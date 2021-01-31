{-# LANGUAGE Strict #-}
module Compiler where

import Data.List
import Data.Maybe
import Text.Pretty.Simple (pPrint, pPrintNoColor)

import Gensym
import AST

type Binding = (String, R0Expr)

------------------------------------------------------------
-- select-instructions
------------------------------------------------------------

data X86Arg = VarXE String
            | DerefE String Int
            | RegE String
            | IntXE Int
  deriving (Eq, Ord, Show)

data X86Instr = MovqE X86Arg X86Arg
              | AddqE X86Arg X86Arg
              | CallqE String
              | RetqE
  deriving (Eq, Ord, Show)


siInt :: R0Expr -> X86Arg
siInt (IntE i) = undefined

siTail :: R0Expr -> [X86Instr]
siTail e = undefined

------------------------------------------------------------
-- print-x86
------------------------------------------------------------

macos :: Bool
macos = False

printFun :: String -> String
printFun s = case macos of
  True -> "_" ++ s
  False -> s

printX86Arg :: X86Arg -> String
printX86Arg e = undefined

printX86Instr :: X86Instr -> String
printX86Instr e = undefined

printX86 :: [X86Instr] -> String
printX86 ss = undefined

------------------------------------------------------------
-- compile / main
------------------------------------------------------------

compile :: R0Expr -> String
compile = printX86 . siTail

logOutput :: Show b => String -> (a -> b) -> (a -> IO b)
logOutput name f = \ x -> do
  let result = f x
  putStrLn "--------------------------------------------------"
  putStrLn $ "Output of pass " ++ name ++ ":"
  putStrLn "--------------------------------------------------"
  pPrintNoColor result
  putStrLn ""
  return result

compileLog :: R0Expr -> IO String
compileLog e =
  (logOutput "input" id) e >>=
  (logOutput "siTail" siTail) >>=
  (logOutput "printX86" printX86)



