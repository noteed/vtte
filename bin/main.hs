{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)


------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> kilo fn
    _ -> do
      putStrLn "Usage: vtte <filename>"
      exitFailure


------------------------------------------------------------------------------
foreign import ccall unsafe "kilo"
  c_kilo :: CString -> IO ()

kilo :: String -> IO ()
kilo fn = withCString fn c_kilo
