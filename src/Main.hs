module Main(main) where

import System.Environment
import System.IO

import ImperativeCode
import LinearMatrixCode
import Parser

main :: IO ()
main = do
  putStr "Enter the name of the computation file: "
  fileName <- getLine
  fileHandle <- openFile fileName ReadMode
  programText <- hGetContents fileHandle
  putStrLn programText
  let compiledProg = compile programText
      outFileName = cFileName fileName
  writeFile outFileName compiledProg
  hClose fileHandle
  putStrLn compiledProg
  
compile :: String -> String
compile = prettyC . toImperativeCode . toLinearMatrixCode . parseComputation

cFileName :: String -> String
cFileName name = (takeWhile (/='.') name) ++ ".c"
