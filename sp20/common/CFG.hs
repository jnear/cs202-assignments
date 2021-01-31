{-# LANGUAGE Strict, TypeSynonymInstances, FlexibleInstances #-}
module CFG where

import Data.IORef
import System.IO.Unsafe

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type CFG a = IORef (Map String a)

instance Show a => Show (CFG a) where
  show = showCFG

emptyCFG :: () -> CFG a
{-# NOINLINE emptyCFG #-}
emptyCFG _ = unsafePerformIO (newIORef Map.empty)

addCFGNode :: CFG a -> String -> a -> ()
{-# NOINLINE addCFGNode #-}
addCFGNode g label block = unsafePerformIO $ do
  gRep <- readIORef g
  writeIORef g (Map.insert label block gRep)

lookupCFG :: CFG a -> String -> Maybe a
{-# NOINLINE lookupCFG #-}
lookupCFG g label = unsafePerformIO $ do
  gRep <- readIORef g
  return $ Map.lookup label gRep

toListCFG :: CFG a -> [(String, a)]
{-# NOINLINE toListCFG #-}
toListCFG g = unsafePerformIO $ do
  gRep <- readIORef g
  return $ Map.toList gRep

showCFG :: (Show a) => CFG a -> String
{-# NOINLINE showCFG #-}
showCFG g = unsafePerformIO $ do
  gRep <- readIORef g
  return $ show $ Map.toList gRep


