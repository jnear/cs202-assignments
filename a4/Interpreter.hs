{-# LANGUAGE Strict #-}
module Interpreter where

import System.Environment
import AST
import Parser hiding (main)

type Env = [(String, Val)]

data Val = IntVal Int | BoolVal Bool
  deriving (Eq, Ord, Show)

eval :: R2Expr -> Env -> Val
eval e env = case e of
  IntE i -> IntVal i
  VarE x -> case lookup x env of
    Just i -> i
    Nothing -> error $ "Failed to find variable " ++ (show x) ++ " in environment " ++ (show env)
  PlusE e1 e2 ->
    let IntVal i1 = eval e1 env
        IntVal i2 = eval e2 env
    in IntVal (i1 + i2)
  LetE x e1 e2 ->
    let v1 = eval e1 env
        env' = (x, v1) : env in
      eval e2 env'
  OrE e1 e2 ->
    let BoolVal v1 = eval e1 env
        BoolVal v2 = eval e2 env
    in BoolVal (v1 || v2)
  AndE e1 e2 ->
    let BoolVal v1 = eval e1 env
        BoolVal v2 = eval e2 env
    in BoolVal (v1 && v2)
  CmpE c e1 e2 ->
    let v1 = eval e1 env
        v2 = eval e2 env
    in case c of
      CmpEqual -> BoolVal (v1 == v2)
      CmpLTE -> BoolVal (v1 <= v2)
      CmpGTE -> BoolVal (v1 >= v2)
      CmpGT -> BoolVal (v1 > v2)
      CmpLT -> BoolVal (v1 < v2)
  IfE e1 e2 e3 ->
    let v1 = eval e1 env
    in case v1 of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error ("Non-boolean test: " ++ (show v1))
  TrueE -> BoolVal True
  FalseE -> BoolVal False
  NotE e1 ->
    let BoolVal v1 = eval e1 env
    in BoolVal (not v1)



main :: IO ()
main = do
  [fileName] <- getArgs

  putStrLn "============================================================"
  putStrLn $ "Interpreting the file: " ++ (show fileName)
  putStrLn "============================================================"

  programAST <- parseFile (fileName)
  putStrLn $ show $ eval programAST []
