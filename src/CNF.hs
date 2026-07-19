-- CNF: Conjunctive Normal Form

module CNF where

import Control.Monad.ST (runST)
import Data.List (intercalate, sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import Data.Vector.Algorithms.Search (binarySearch)

import Expr
import Util

type VarMap = V.Vector String

type Variable = Int
type Literal = Variable
type Clause = [Literal]
data CNF = CNF
  { cnf_n_vars :: Int
  , cnf_n_clauses :: Int
  , cnf_var_map :: VarMap
  , cnf_clauses :: [Clause]
  }

showClause :: Clause -> String
showClause clause =
    let lits_str = intercalate "," $ map show clause
    in "{" ++ lits_str ++ "}"

instance Show CNF where
  show (CNF _ _ _ clauses) =
    let clauses_str = intercalate "," $ map showClause clauses
    in "{" ++ clauses_str ++ "}"

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
exprToClause var_map (Expr_Or es) = mapM (exprToLit var_map) es
exprToClause var_map expr_lit =
  case exprToLit var_map expr_lit of
    Nothing -> Nothing
    Just lit -> Just [lit]

-- Converts Expr to CNF. Sorts literals of each clause, and sorts clauses.
exprToCnf :: Expr -> Either ErrorMsg CNF
exprToCnf expr
  | not $ exprIsCnf expr = Left "exprToCnf: input Expr not in CNF"
  | exprIsLit expr = Right $ go (Expr_And [Expr_Or [expr]])
  | exprIsClause expr = Right $ go (Expr_And [expr])
  | otherwise {- Expr_And -} = Right $ go expr
  where
    go e =
      let var_map = strListToVarMap $ exprVars e
          expr_clauses = exprClauses e
          clauses = sort $ map sort $ fromJust $
                    mapM (exprToClause var_map) expr_clauses
      in CNF { cnf_n_vars = length var_map
             , cnf_n_clauses = length expr_clauses
             , cnf_var_map = var_map
             , cnf_clauses = clauses }

clauseTrivial :: Clause -> Bool
clauseTrivial [] = False
clauseTrivial (lit:lits) = elem (-lit) lits || clauseTrivial lits

cnfNonTrivialClauses :: CNF -> CNF
cnfNonTrivialClauses (CNF n_vars _ var_map clauses) =
  CNF n_vars (length new_clauses) var_map new_clauses
  where new_clauses = filter (not . clauseTrivial) clauses

litsUnique :: Clause -> Clause
litsUnique [] = []
litsUnique (l:ls)
  | elem l ls = litsUnique ls
  | otherwise = l : litsUnique ls

cnfUniqueLits :: CNF -> CNF
cnfUniqueLits (CNF n_vars _ var_map clauses) =
  CNF n_vars (length new_clauses) var_map new_clauses
  where new_clauses = map litsUnique clauses

clauseUnique :: [Clause] -> Clause -> Bool
clauseUnique clauses targ = not $ elem targ clauses

clausesUnique :: [Clause] -> [Clause]
clausesUnique [] = []
clausesUnique (c:cs)
  | elem c cs = clausesUnique cs
  | otherwise = c : clausesUnique cs

cnfUniqueClauses :: CNF -> CNF
cnfUniqueClauses (CNF n_vars _ var_map clauses) =
  CNF n_vars (length new_clauses) var_map new_clauses
  where new_clauses = clausesUnique clauses
