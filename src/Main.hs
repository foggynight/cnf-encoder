module Main where

import System.IO (hPutStrLn, stderr)

import CNF
import Expr
import Parser

main' :: Expr -> IO ()
main' expr = do
  hPutStrLn stderr $ "Expression: " ++ show expr
  hPutStrLn stderr "DIMACS CNF:"
  case exprToCnf expr of
    Left err  -> hPutStrLn stderr err
    Right cnf -> putStr $ dimacsCnf cnf

main :: IO ()
main = do
  input <- getContents
  case parse input of
    Left err   -> hPutStrLn stderr err
    Right expr -> main' expr
