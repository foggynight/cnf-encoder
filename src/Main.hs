module Main where

import Control.Monad (when)
import System.IO (hPutStrLn, stderr)

import CNF
import Expr
import Parser
import Util

mainConvertToCnf :: Expr -> IO Expr
mainConvertToCnf expr = do
  let nbicon_expr = exprElimBicon expr
  hPutStrLn stderr $ "Eliminated Biconditionals: " ++ show nbicon_expr

  let nimply_expr = exprElimImply nbicon_expr
  hPutStrLn stderr $ "Eliminated Implications:   " ++ show nimply_expr

  let flat_expr = exprFlatten nimply_expr
  hPutStrLn stderr $ "Flattened NOT/AND/OR:      " ++ show flat_expr

  let nnf_expr = exprFlatten $ exprToNnf flat_expr
  hPutStrLn stderr $ "Negation Normal Form:      " ++ show nnf_expr

  let cnf_expr = exprFlatten $ exprOrOverAnd nnf_expr
  hPutStrLn stderr $ "Distributed OR over AND:   " ++ show cnf_expr

  let final_expr = cnf_expr
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
    else do hPutStrLn stderr   "Converting expression to CNF..."
            hPutStrLn stderr $ "Initial Expression:        " ++ show expr
            mainConvertToCnf expr
  hNewline stderr

  hPutStrLn stderr "DIMACS CNF:"
  case exprToCnf final_expr of
    Left err  -> hPutStrLn stderr $ "error: " ++ err
    Right cnf -> putStr $ dimacsCnf cnf

main :: IO ()
main = do
  input <- getContents
  hPutStrLn stderr $ "Input:      " ++ input
  case parse input of
    Left err   -> hPutStrLn stderr $ "error: " ++ err
    Right expr -> mainExprToDimacs expr
