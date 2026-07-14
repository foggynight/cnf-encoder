module Main where

import System.IO (hPutStrLn, stderr)

import CNF
import Expr
import Parser
import Util

mainConvertToCnf :: Expr -> IO Expr
mainConvertToCnf expr = do
  let flat_expr = exprFlatten expr
  hPutStrLn stderr $ "Flattened AND/OR: " ++ show flat_expr
  let dm_expr = exprDeMorgans flat_expr
  hPutStrLn stderr $ "DeMorgan's Laws:  " ++ show dm_expr
  pure flat_expr

main' :: Expr -> IO ()
main' expr = do
  hPutStrLn stderr $ "Expression: " ++ show expr
  hNewline stderr

  final_expr <-
    if exprIsCnf expr
    then do hPutStrLn stderr "Expression is already in CNF."
            pure expr
    else do hPutStrLn stderr "Converting expression to CNF..."
            mainConvertToCnf expr
  hNewline stderr

  hPutStrLn stderr "DIMACS CNF:"
  case exprToCnf final_expr of
    Left err  -> hPutStrLn stderr err
    Right cnf -> putStr $ dimacsCnf cnf

main :: IO ()
main = do
  input <- getContents
  hPutStrLn stderr $ "Input: " ++ input
  case parse input of
    Left err   -> hPutStrLn stderr err
    Right expr -> main' expr
