{-# LANGUAGE Strict #-}
module Interpreter where

import System.Environment
import AST
import Parser hiding (main)

eval :: R0Expr -> Int
eval e = case e of
  IntE i -> i
  PlusE e1 e2 -> (eval e1) + (eval e2)

main :: IO ()
main = do
  [fileName] <- getArgs

  putStrLn "============================================================"
  putStrLn $ "Interpreting the file: " ++ (show fileName)
  putStrLn "============================================================"

  programAST <- parseFile (fileName)
  putStrLn $ show $ eval programAST
