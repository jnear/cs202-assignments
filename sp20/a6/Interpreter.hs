{-# LANGUAGE Strict #-}
module Interpreter where

import System.Environment
import Data.Maybe
import AST
import Parser hiding (main)

type Env = [(String, Val)]

data Val = IntVal Int | BoolVal Bool | AddrVal Addr | VoidVal | ClosureVal [String] R4Expr Env
  deriving (Eq, Ord, Show)

data VectorVal = Vec [Val]
  deriving (Eq, Ord, Show)

type Addr = Int
type Store = [(Addr, VectorVal)]

eval :: R4Expr -> Env -> Store -> (Val, Store)
eval e env sto = case e of
  IntE i -> (IntVal i, sto)
  VarE x -> case lookup x env of
    Just i -> (i, sto)
    Nothing -> error $ "Failed to find variable " ++ (show x) ++ " in environment " ++ (show env)
  PlusE e1 e2 ->
    let (IntVal i1, sto1) = eval e1 env sto
        (IntVal i2, sto2) = eval e2 env sto1
    in (IntVal (i1 + i2), sto2)
  LetE x e1 e2 ->
    let (v1, sto1) = eval e1 env sto
        env' = (x, v1) : env in
      eval e2 env' sto1
  OrE e1 e2 ->
    let (BoolVal v1, sto1) = eval e1 env sto
        (BoolVal v2, sto2) = eval e2 env sto1
    in (BoolVal (v1 || v2), sto2)
  AndE e1 e2 ->
    let (BoolVal v1, sto1) = eval e1 env sto
        (BoolVal v2, sto2) = eval e2 env sto1
    in (BoolVal (v1 && v2), sto2)
  CmpE c e1 e2 ->
    let (v1, sto1) = eval e1 env sto
        (v2, sto2) = eval e2 env sto1
        val = case c of
                CmpEqual -> BoolVal (v1 == v2)
                CmpLTE -> BoolVal (v1 <= v2)
                CmpGTE -> BoolVal (v1 >= v2)
                CmpGT -> BoolVal (v1 > v2)
                CmpLT -> BoolVal (v1 < v2)
    in (val, sto2)
  IfE e1 e2 e3 ->
    let (v1, sto1) = eval e1 env sto
    in case v1 of
      BoolVal True -> eval e2 env sto1
      BoolVal False -> eval e3 env sto1
      _ -> error ("Non-boolean test: " ++ (show v1))
  TrueE -> (BoolVal True, sto)
  FalseE -> (BoolVal False, sto)
  NotE e1 ->
    let (BoolVal v1, sto1) = eval e1 env sto
    in (BoolVal (not v1), sto1)
  VectorE args ->
    let (vals, sto1) = evalArgs args env sto
        newAddr      = length sto1
        newVal       = Vec vals
        sto2         = (newAddr, newVal) : sto1
    in (AddrVal newAddr, sto2)
  VectorRefE e1 i ->
    let (AddrVal a, sto1) = eval e1 env sto
        Vec vals          = fromJust (lookup a sto1)
        val               = vals !! i
    in (val, sto1)
  VectorSetE e1 i e2 ->
    let (AddrVal a, sto1) = eval e1 env sto
        (val, sto2)       = eval e2 env sto1
        Vec origVals      = fromJust (lookup a sto2)
        newVals           = replace i val origVals
        sto3              = updateStore sto2 a (Vec newVals)
    in (VoidVal, sto3)
  VoidE -> (VoidVal, sto)
  FunCallE f args ->
    let (ClosureVal argNames body cEnv, sto1) = eval f env sto
        (vals, sto2) = evalArgs args env sto1
        bodyEnv = (zip argNames vals) ++ cEnv
    in eval body bodyEnv sto2
    
  _ -> error (show e)

mkDef :: Env -> R4Definition -> (String, Val)
mkDef env (Defn name argPs _ body) =
  (name, ClosureVal (map fst argPs) body env)

buildDefEnv :: [R4Definition] -> Env
buildDefEnv defs =
  let env = map (mkDef env) defs
  in env

evalArgs :: [R4Expr] -> Env -> Store -> ([Val], Store)
evalArgs [] env sto = ([], sto)
evalArgs (e : es) env sto =
  let (vals, sto1) = evalArgs es env sto
      (v, sto2) = eval e env sto1
  in (v : vals, sto2)

updateStore :: Store -> Addr -> VectorVal -> Store
updateStore sto a v = (a, v) : (filter ((a /=).fst) sto)

replace :: Int -> Val -> [Val] -> [Val]
replace index val = map (\(index', val') -> if index' == index then val else val') . zip [0..]

evalProgram :: R4Program -> (Val, Store)
evalProgram (defs, e) =
  let initEnv = buildDefEnv defs
  in eval e initEnv []

main :: IO ()
main = do
  [fileName] <- getArgs

  putStrLn "============================================================"
  putStrLn $ "Interpreting the file: " ++ (show fileName)
  putStrLn "============================================================"

  programAST <- parseFile (fileName)
  putStrLn $ show $ evalProgram programAST
