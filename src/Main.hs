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
  let nbicon_expr = exprElimBicon expr
  let nimply_expr = exprElimImply nbicon_expr
  let nnf_expr    = exprToNnf nimply_expr
  let flat_expr   = exprFlatten nnf_expr
  let ntriv_expr  = {- exprElimTriv -} flat_expr
  let cnf_expr    = exprFlatten $ exprOrOverAnd ntriv_expr
  let final_expr  = cnf_expr

  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Biconditionals:") ++ show nbicon_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Implications:") ++ show nimply_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Negation Normal Form:") ++ show nnf_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Flattened NOT/AND/OR:") ++ show flat_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Trivial Clauses:") ++ show flat_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Distributed OR over AND:") ++ show cnf_expr

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
            hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Initial Expression:") ++ show expr
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
