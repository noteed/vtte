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
    [fn] -> do
      initialize
      setSyntaxHighlighting fn
      loadFile fn
      enableRawMode 0 -- stdin
      setStatusMessage "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find"
      kilo fn
    _ -> do
      putStrLn "Usage: vtte <filename>"
      exitFailure


------------------------------------------------------------------------------
kilo :: String -> IO ()
kilo fn = withCString fn c_kilo

foreign import ccall unsafe "kilo"
  c_kilo :: CString -> IO ()


-- Initialize the editor state. In particular, query the system to obtain the
-- terminal width and height.
foreign import ccall unsafe "initEditor"
  initialize :: IO ()


-- Modify the editor state to setup the syntax highlighting scheme, based on
-- the given filename.
setSyntaxHighlighting :: String -> IO ()
setSyntaxHighlighting fn = withCString fn c_setSyntaxHighlighting

foreign import ccall unsafe "editorSelectSyntaxHighlight"
  c_setSyntaxHighlighting :: CString -> IO ()


-- Load a file into the editor state.
loadFile :: String -> IO ()
loadFile fn = withCString fn c_loadFile

foreign import ccall unsafe "editorOpen"
  c_loadFile :: CString -> IO ()


-- Set terminal to raw mode.
foreign import ccall unsafe "enableRawMode"
  enableRawMode :: Int -> IO ()


-- Write a string to the status line.
-- In C, this uses a va_list to format a number of arguments.
setStatusMessage :: String -> IO ()
setStatusMessage fmt = withCString fmt c_setStatusMessage

foreign import ccall unsafe "editorSetStatusMessage"
  c_setStatusMessage :: CString -> IO ()
