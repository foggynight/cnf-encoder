module DIMACS where

import CNF

dimacsHeaderLine :: Int -> Int -> String
dimacsHeaderLine n m = "p cnf " ++ show n ++ " " ++ show m

dimacsVarMap :: VarMap -> String
dimacsVarMap var_map = "c VarMap: " ++ (filter (/= '"') $ show var_map)

dimacsClauseLine :: Clause -> String
dimacsClauseLine [] = "0"
dimacsClauseLine (lit:lits) = show lit ++ " " ++ dimacsClauseLine lits

dimacsCnf :: CNF -> String
dimacsCnf (CNF n_vars n_clauses var_map clauses) =
  let var_names_line = dimacsVarMap var_map
      header_line = dimacsHeaderLine n_vars n_clauses
      clause_lines = map dimacsClauseLine clauses
  in unlines $ var_names_line : header_line : clause_lines
