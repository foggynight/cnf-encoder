module DIMACS where

import Data.List (intercalate)
import qualified Data.Vector as V (toList)

import CNF

dimacsHeaderLine :: Int -> Int -> String
dimacsHeaderLine n m = "p cnf " ++ show n ++ " " ++ show m

dimacsVarMap :: VarMap -> String
dimacsVarMap var_map = "{" ++ map_str ++ "}"
  where map_str = intercalate "," $ go 1 (V.toList var_map)
        go :: Int -> [String] -> [String]
        go _ [] = []
        go i (v:vs) = (show i ++ ":" ++ id v) : go (i + 1) vs

dimacsClauseLine :: Clause -> String
dimacsClauseLine [] = "0"
dimacsClauseLine (lit:lits) = show lit ++ " " ++ dimacsClauseLine lits

dimacsCnf :: CNF -> String
dimacsCnf (CNF n_vars n_clauses var_map clauses) =
  let var_map_line = "c VarMap: " ++ dimacsVarMap var_map
      header_line = dimacsHeaderLine n_vars n_clauses
      clause_lines = map dimacsClauseLine clauses
  in unlines $ var_map_line : header_line : clause_lines
