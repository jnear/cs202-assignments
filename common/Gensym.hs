module Gensym where

import Data.IORef
import System.IO.Unsafe

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
