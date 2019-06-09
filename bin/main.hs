{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.C.Types


------------------------------------------------------------------------------
main :: IO ()
main = do
  kilo


------------------------------------------------------------------------------
foreign import ccall unsafe "kilo"
  kilo :: IO ()
