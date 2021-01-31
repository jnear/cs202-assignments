{-# LANGUAGE Strict #-}
module Interpreter where

import System.Environment
import AST
import Parser hiding (main)

type Env = [(String, Int)]

eval :: R1Expr -> Env -> Int
eval e env = case e of
  IntE i -> i
  VarE x -> case lookup x env of
    Just i -> i
    Nothing -> error $ "Failed to find variable " ++ (show x) ++ " in environment " ++ (show env)
  PlusE e1 e2 ->
    (eval e1 env) + (eval e2 env)
  LetE x e1 e2 ->
    let v1 = eval e1 env
        env' = (x, v1) : env in
      eval e2 env'


main :: IO ()
main = do
  [fileName] <- getArgs

  putStrLn "============================================================"
  putStrLn $ "Interpreting the file: " ++ (show fileName)
  putStrLn "============================================================"

  programAST <- parseFile (fileName)
  putStrLn $ show $ eval programAST []
