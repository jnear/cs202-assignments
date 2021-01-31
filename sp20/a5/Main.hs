{-# LANGUAGE Strict #-}
module Main where

import System.Environment
import Parser hiding (main)
import Compiler hiding (main)

main :: IO ()
main = do
  [fileName] <- getArgs

  _ <- putStrLn "============================================================"
  _ <- putStrLn $ "Compiling the file: " ++ (show fileName)
  _ <- putStrLn "============================================================"

  programAST <- parseFile (fileName)
  compiledASM <- compileLog programAST
  -- let compiledASM = compile programAST
  writeFile (fileName ++ ".s") compiledASM
