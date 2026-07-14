module Main where

import Control.Monad (when)
import System.IO (hPutStrLn, stderr)

import CNF
import Expr
import Parser
import Util

mainConvertToCnf :: Expr -> IO Expr
mainConvertToCnf expr = do
  let flat_expr = exprFlatten expr
  hPutStrLn stderr $ "Flattened AND/OR:     " ++ show flat_expr

  let nnf_expr = exprFlatten $ exprToNnf flat_expr
  hPutStrLn stderr $ "Negation Normal Form: " ++ show nnf_expr

  let final_expr = nnf_expr
  when (not $ exprIsCnf final_expr) $
    hPutStrLn stderr $ "Error: Final expression is not in CNF."
  pure final_expr

mainExprToDimacs :: Expr -> IO ()
mainExprToDimacs expr = do
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
    Right expr -> mainExprToDimacs expr
