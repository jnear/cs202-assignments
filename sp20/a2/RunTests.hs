module RunTests where

import System.Directory
import Data.List
import System.Process

import Parser hiding (main)
import Compiler hiding (main)
import Interpreter hiding (main)

logging :: Bool
logging = False

logOut :: String -> IO ()
logOut s | logging   = putStrLn s
         | otherwise = return ()

readInt :: String -> Int
readInt = read

runTest :: String -> IO ()
runTest fileName = do
  putStrLn "============================================================"
  putStrLn fileName
  putStrLn "============================================================"

  e <- parseFile ("tests/" ++ fileName)
  let interpResult = eval e []
  putStrLn $ "Interpreter result: " ++ (show interpResult)

  -- Compile the program
  let r = compile e

  if logging
    then compileLog e
    else return r

  -- Write out the assembly code to a .s file
  let assemblyFileName = "tests/" ++ fileName ++ ".s"
  writeFile assemblyFileName r

  -- Compile the assembly to an executable
  gccOutput <- readProcess "gcc" ["-g", "../runtime.o", assemblyFileName] ""
  putStrLn $ "GCC Output: " ++ gccOutput

  -- Run the executable
  x86Result <- readProcess "./a.out" [] ""
  putStrLn $ "x86 Result: " ++ x86Result

  -- Check that executable result was the same
  let x86ResultInt = readInt x86Result
  case x86ResultInt == interpResult of
    True  -> putStrLn "Passed!"
    False -> putStrLn "FAIL: Results don't match"

  -- Remove the .s file and compiled files
  removeFile assemblyFileName
  removeFile "a.out"
  
  putStrLn ""

main :: IO ()
main = do
  sourceFiles <- listDirectory "tests"
  let files = sort $ filter (".r1" `isSuffixOf`) sourceFiles
  putStrLn $ "Test files: " ++ (show files)
  mapM_ runTest files
  putStrLn "done"
