{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.C.String (withCString, CString)
import Foreign.C.Types


------------------------------------------------------------------------------
main :: IO ()
main = do
  kilo "hello.txt"


------------------------------------------------------------------------------
foreign import ccall unsafe "kilo"
  c_kilo :: CString -> IO ()

kilo :: String -> IO ()
kilo fn = withCString fn c_kilo
