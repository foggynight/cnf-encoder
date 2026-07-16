{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Language.Haskell.TH.Quote (quoteExp)
import System.IO (hPutStrLn, stderr)

import CNF
import Expr
import Parser
import Util

mainConvertToCnf :: Expr -> IO Expr
mainConvertToCnf expr = do
  let nbicon_expr  = exprElimBicon expr
  let nimply_expr  = exprElimImply nbicon_expr
  let nnf_expr     = exprToNnf nimply_expr
  let flat_expr    = exprFlatten nnf_expr
  let clauses_expr = exprFlatten $ exprOrOverAnd flat_expr
  let ntriv_expr   = exprNonTrivialClauses clauses_expr
  let ndup_expr    = {- exprUniqueClauses -} ntriv_expr
  let final_expr   = ndup_expr

  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Eliminated Biconditionals:") ++ show nbicon_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Eliminated Implications:") ++ show nimply_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Negation Normal Form:") ++ show nnf_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Flattened NOT/AND/OR:") ++ show flat_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Distributed OR over AND:") ++ show clauses_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Eliminated Trivial Clauses:") ++ show ntriv_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 30 ' ') "Removed Duplicate Clauses:") ++ show ndup_expr

  hPutStrLn stderr $ "\nFinal CNF: " ++ show final_expr
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
    else do hPutStrLn stderr $ "Converting expression to CNF...\n"
              ++ $(quoteExp (padRightQQ 30 ' ') "Initial Expression:")
              ++ show expr
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
