module Main where

import Control.Monad
import Text.Printf
import System.Environment
import System.Exit

import Graphics.FDL.Parser
import Graphics.FDL.Typer
import Graphics.FDL.Backend.GL

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) $ putStrLn "Usage: draw picture.fdl" >> exitFailure
    source <- readFile (head args)
    let 
      (ast, parserErrors) = parseProg source
      typerResult         = typeProg  ast
    when (not $ null parserErrors) $ showParserErrors source parserErrors >> exitFailure
    case typerResult of
      TyperErrors es -> showTyperErrors source es >> exitFailure
      TyperSuccess p -> compile p >>= run

showParserErrors :: String -> [Error Pos] -> IO ()
showParserErrors src es = mapM_ (showParserError src) es

showParserError :: String -> Error Pos -> IO ()
showParserError src (Inserted text pos _) = 
    showError src "Parse" ("Something seems to be missing, perhaps " ++ text) pos
showParserError src (Deleted text pos _) = 
    showError src "Parse" ("Unexpected charater " ++ text) pos
showParserError _   (DeletedAtEnd text) = do
    putStrLn "Unexpected text at the end of the file:"
    putStrLn text
    putStrLn ""

showTyperErrors :: String -> [TyperError] -> IO ()
showTyperErrors src es = mapM_ (showTyperError src) es

showTyperError :: String -> TyperError -> IO ()
showTyperError src (TyperError text pos) = 
    showError src "Typer" text pos

showError :: String -> String -> String -> Pos -> IO ()
showError src error msg pos = do
    putStrLn $ error ++ " error at " ++ showPos pos
    putStrLn $ msg
    putStrLn $ lines src !! (posLine pos - 1)
    putStrLn $ replicate (posCol pos - 1) ' ' ++ "^"
    putStrLn $ ""

showPos :: Pos -> String
showPos (Pos _ line col) = printf "line %d column %d" line col
