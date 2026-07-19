{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Language.Haskell.TH.Quote (quoteExp)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import CNF
import DIMACS
import Expr
import Parser
import Util

mainConvertToCnf' :: Expr -> IO (Maybe CNF)
mainConvertToCnf' expr = do
  let nbicon_expr = exprElimBicon expr
  let nimply_expr = exprElimImply nbicon_expr
  let nnf_expr    = exprToNnf nimply_expr
  let flat_expr   = exprFlatten nnf_expr  -- TODO: What if you or-over-and non-flat expr?
  let cnf_expr    = exprFlatten $ exprOrOverAnd flat_expr

  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Initial Expression:") ++ show expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Biconditionals:") ++ show nbicon_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Implications:") ++ show nimply_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Negation Normal Form:") ++ show nnf_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Flattened NOT/AND/OR:") ++ show flat_expr
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Distributed OR over AND:") ++ show cnf_expr

  if not $ exprIsCnf cnf_expr
  then do hPutStrLn stderr $ "Error: Expression is not in CNF."
          pure Nothing
  else case exprToCnf cnf_expr of
         Left msg -> do hPutStrLn stderr $
                          "Error: Failed to convert to CNF type:" ++ msg
                        pure Nothing
         Right cnf -> pure $ Just cnf

mainConvertToCnf :: Expr -> IO (Maybe CNF)
mainConvertToCnf expr
  | exprIsCnf expr = do
      hPutStrLn stderr "Expression already in CNF."
      case exprToCnf expr of
        Left msg -> do hPutStrLn stderr $
                         "Error: Failed to convert to CNF type:" ++ msg
                       pure Nothing
        Right cnf -> pure $ Just cnf
  | otherwise = mainConvertToCnf' expr

mainSimplifyCnf :: CNF -> IO CNF
mainSimplifyCnf cnf = do
  let ntriv_cnf = cnfNonTrivialClauses cnf
  let uniql_cnf = cnfUniqueLits ntriv_cnf
  let uniqc_cnf = cnfUniqueClauses uniql_cnf
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Initial CNF:") ++ show cnf
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Trivial Clauses:") ++ show ntriv_cnf
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Duplicate Literals:") ++ show uniql_cnf
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Eliminated Duplicate Clauses:") ++ show uniqc_cnf
  pure uniqc_cnf

mainExprToDimacs :: Expr -> IO ()
mainExprToDimacs expr = do
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Expression:") ++ show expr
  hNewline stderr

  maybe_init_cnf <- do hPutStrLn stderr $ "Converting to CNF..."
                       mainConvertToCnf expr
  hNewline stderr

  when (isNothing maybe_init_cnf) $ exitFailure
  let init_cnf = fromJust maybe_init_cnf

  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Variable to Index Map:")
                     ++ dimacsVarMap (cnf_var_map init_cnf)
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Initial CNF (Set Notation):")
                     ++ show init_cnf
  hNewline stderr

  final_cnf <- do hPutStrLn stderr "Simplifying CNF..."
                  mainSimplifyCnf init_cnf
  hNewline stderr

  hPutStrLn stderr $
    $(quoteExp (padRightQQ 32 ' ') "Final CNF (Set Notation):")
    ++ show final_cnf
  hNewline stderr

  hPutStrLn stderr "DIMACS CNF:"
  putStr $ dimacsCnf final_cnf

main :: IO ()
main = do
  input <- getContents
  hPutStrLn stderr $ $(quoteExp (padRightQQ 32 ' ') "Input:") ++ input
  case parse input of
    Left err   -> hPutStrLn stderr $ "error: " ++ err
    Right expr -> mainExprToDimacs expr
