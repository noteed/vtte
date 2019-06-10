{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign.C.String (withCString, CString)
import Foreign.C.Types
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)


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
      refresh'
      loop
    _ -> do
      putStrLn "Usage: vtte <filename>"
      exitFailure

loop = do
  k <- readKeypress
  if k == ctrl_c
    then do
      refresh'
      exitSuccess
    else do
      processKeypress k
      refresh
      loop


------------------------------------------------------------------------------
refresh' = do
  h <- getScreenHeight
  putStr "\x1b[?25l"  -- Hide cursor.
  putStr "\x1b[H"     -- Go to upper left.
  putStr "        Vtte editor -- version 0.0.0\x1b[0K\r\n"
  mapM_ (\_ -> putStr "~\x1b[0K\r\n") [2 .. h-1]
  putStr "~\x1b[0K"   -- Last line, don't \r\n.
  putStr "\x1b[H"
  putStr "\x1b[?25h"  -- Show cursor.
  hFlush stdout
  --
  -- Is it possible (or necessary) to  use a single write() for performance or
  -- to avoid glitches ?
  -- Or is the Haskell buffer management with putStr + hFlush good enough ?


------------------------------------------------------------------------------
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


-- Set terminal to raw mode. This also registers an atexit handler to disable
-- raw mode.
foreign import ccall unsafe "enableRawMode"
  enableRawMode :: Int -> IO ()


-- Write a string to the status line.
-- In C, this uses a va_list to format a number of arguments.
setStatusMessage :: String -> IO ()
setStatusMessage fmt = withCString fmt c_setStatusMessage

foreign import ccall unsafe "editorSetStatusMessage"
  c_setStatusMessage :: CString -> IO ()


-- Redraw the whole screen.
foreign import ccall unsafe "editorRefreshScreen"
  refresh :: IO ()

-- Read a key, which can be processed by `processKeypress`.
foreign import ccall unsafe "editorReadKey"
  readKeypress :: IO Int

-- Process one key press.
foreign import ccall unsafe "editorProcessKeypress"
  processKeypress :: Int -> IO ()


foreign import ccall unsafe "getScreenHeight"
  getScreenHeight :: IO Int


------------------------------------------------------------------------------
ctrl_c = 3
ctrl_q = 17
