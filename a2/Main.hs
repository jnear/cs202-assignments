{-# LANGUAGE Strict #-}
module Main where

import System.Environment
import Parser hiding (main)
import Compiler hiding (main)

main :: IO ()
main = do
  [fileName] <- getArgs

  putStrLn "============================================================"
  putStrLn $ "Compiling the file: " ++ (show fileName)
  putStrLn "============================================================"

  programAST <- parseFile (fileName)
  compiledASM <- compileLog programAST
  writeFile (fileName ++ ".s") compiledASM
