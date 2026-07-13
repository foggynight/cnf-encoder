module CNF where

import Data.Maybe (fromJust)

import Expr

type Literal = String  -- TEMP: will be Int
type Clause = [Literal]
data CNF = CNF
  { cnf_n_vars :: Int
  , cnf_n_clauses :: Int
  , cnf_var_names :: [String]
  , cnf_clauses :: [Clause]
  } deriving (Show)

exprToLit :: Expr -> Maybe Literal
exprToLit (Expr_Var var) = Just var
exprToLit (Expr_Not e) = case exprToLit e of
                           Nothing -> Nothing
                           Just var -> Just $ '-' : var
exprToLit _ = Nothing

exprToClause :: Expr -> Maybe Clause
exprToClause (Expr_And _) = Nothing
exprToClause (Expr_Or es) = mapM exprToLit es
exprToClause expr_lit = case exprToLit expr_lit of
                          Nothing -> Nothing
                          Just lit -> Just [lit]

exprToCnf :: Expr -> Either String CNF
exprToCnf expr
  | not $ exprIsCnf expr = Left "error: exprToCnf: input Expr not in CNF"
  | exprIsLit expr = Right $ go (Expr_And [Expr_Or [expr]])
  | exprIsClause expr = Right $ go (Expr_And [expr])
  | otherwise {- Expr_And -} = Right $ go expr
  where go e =
          let var_names = exprVars e
              expr_clauses = exprClauses e
          in CNF { cnf_n_vars = length var_names
                 , cnf_n_clauses = length expr_clauses
                 , cnf_var_names = var_names
                 , cnf_clauses = fromJust $ mapM exprToClause expr_clauses }

-- Does not contain trailing newline.
dimacsHeaderLine :: Int -> Int -> String
dimacsHeaderLine n m = "p cnf " ++ show n ++ " " ++ show m

dimacsVarNames :: [String] -> String
dimacsVarNames var_names = "c " ++ (filter (/= '"') $ show var_names)

dimacsClauseLine :: Clause -> String
dimacsClauseLine [] = "0"
dimacsClauseLine (lit:lits) = lit ++ " " ++ dimacsClauseLine lits

dimacsCnf :: CNF -> String
dimacsCnf (CNF n_vars n_clauses var_names clauses) =
  let var_names_line = dimacsVarNames var_names
      header_line = dimacsHeaderLine n_vars n_clauses
      clause_lines = map dimacsClauseLine clauses
  in unlines $ var_names_line : header_line : clause_lines
