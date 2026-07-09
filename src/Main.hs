module Main where

--import Expr
import Parser

main :: IO ()
main = do
  input <- getContents
  let tokens = lexStr input
  print tokens
