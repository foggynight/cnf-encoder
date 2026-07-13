module CNF where

import Control.Monad.ST (runST)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search (binarySearch)

import Expr

type VarMap = V.Vector String

type Variable = Int
type Literal = Variable
type Clause = [Literal]
data CNF = CNF
  { cnf_n_vars :: Int
  , cnf_n_clauses :: Int
  , cnf_var_map :: VarMap
  , cnf_clauses :: [Clause]
  } deriving (Show)

strListToVarMap :: [String] -> VarMap
strListToVarMap = V.fromList

-- Finds `var` String in VarMap, and returns index + 1 (0 is invalid variable).
varMapFind :: VarMap -> String -> Variable
varMapFind var_map var = runST $ do
  mut_vec <- V.thaw var_map
  idx <- binarySearch mut_vec var
  pure (idx + 1)

exprToLit :: VarMap -> Expr -> Maybe Literal
exprToLit var_map (Expr_Var var) = Just $ varMapFind var_map var
exprToLit var_map (Expr_Not e) =
  case exprToLit var_map e of
    Nothing -> Nothing
    Just var -> Just $ -var
exprToLit _ _ = Nothing

exprToClause :: VarMap -> Expr -> Maybe Clause
exprToClause _ (Expr_And _) = Nothing
exprToClause var_map (Expr_Or es)  = mapM (exprToLit var_map) es
exprToClause var_map expr_lit =
  case exprToLit var_map expr_lit of
    Nothing -> Nothing
    Just lit -> Just [lit]

exprToCnf :: Expr -> Either String CNF
exprToCnf expr
  | not $ exprIsCnf expr = Left "error: exprToCnf: input Expr not in CNF"
  | exprIsLit expr = Right $ go (Expr_And [Expr_Or [expr]])
  | exprIsClause expr = Right $ go (Expr_And [expr])
  | otherwise {- Expr_And -} = Right $ go expr
  where
    go e =
      let var_map = strListToVarMap $ exprVars e
          expr_clauses = exprClauses e
          clauses = fromJust $ mapM (exprToClause var_map) expr_clauses
      in CNF { cnf_n_vars = length var_map
             , cnf_n_clauses = length expr_clauses
             , cnf_var_map = var_map
             , cnf_clauses = clauses }

dimacsHeaderLine :: Int -> Int -> String
dimacsHeaderLine n m = "p cnf " ++ show n ++ " " ++ show m

dimacsVarMap :: VarMap -> String
dimacsVarMap var_map = "c " ++ (filter (/= '"') $ show var_map)

dimacsClauseLine :: Clause -> String
dimacsClauseLine [] = "0"
dimacsClauseLine (lit:lits) = show lit ++ " " ++ dimacsClauseLine lits

dimacsCnf :: CNF -> String
dimacsCnf (CNF n_vars n_clauses var_map clauses) =
  let var_names_line = dimacsVarMap var_map
      header_line = dimacsHeaderLine n_vars n_clauses
      clause_lines = map dimacsClauseLine clauses
  in unlines $ var_names_line : header_line : clause_lines
