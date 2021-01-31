module Gensym where

import Data.IORef
import System.IO.Unsafe
import System.Random

------------------------------------------------------------
-- gensym (don't look)
------------------------------------------------------------
gensymNum :: IORef Int
{-# NOINLINE gensymNum #-}
gensymNum = unsafePerformIO (newIORef 1)

gensym :: String -> String
{-# NOINLINE gensym #-}
gensym s = unsafePerformIO $ do
  n <- readIORef gensymNum
  writeIORef gensymNum (n+1)
  return $ s ++ (show n)

------------------------------------------------------------
-- read (don't look)
------------------------------------------------------------
readLine :: String
{-# NOINLINE readLine #-}
readLine = unsafePerformIO $ do
  s <- getLine
  return s

------------------------------------------------------------
-- trace (don't look)
------------------------------------------------------------
trace :: String -> a -> a
{-# NOINLINE trace #-}
trace msg v = unsafePerformIO $ do
  putStrLn msg
  return v

------------------------------------------------------------
-- random (don't look)
------------------------------------------------------------
randInt :: (Int, Int) -> Int
{-# NOINLINE randInt #-}
randInt p = unsafePerformIO $ randomRIO p


