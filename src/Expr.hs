module Expr where

import Data.List (intercalate)
import Data.Maybe (fromJust, isNothing)

import Util

data Expr
  = Expr_Var String
  | Expr_Not Expr
  | Expr_And [Expr]
  | Expr_Or [Expr]

instance Show Expr where
  show (Expr_Var var) = var
  show (Expr_Not expr) = "(- " ++ show expr ++ ")"
  show (Expr_And es) = "(*" ++ concat (map (\e -> " " ++ show e) es) ++ ")"
  show (Expr_Or es) = "(+" ++ concat (map (\e -> " " ++ show e) es) ++ ")"

exprIsVar :: Expr -> Bool
exprIsVar (Expr_Var _) = True
exprIsVar _ = False

exprIsLit :: Expr -> Bool
exprIsLit (Expr_Var _) = True
exprIsLit (Expr_Not e) = exprIsVar e
exprIsLit _ = False

exprIsClause :: Expr -> Bool
exprIsClause (Expr_Or es) = and $ map exprIsLit es
exprIsClause expr = exprIsLit expr

exprIsCnf :: Expr -> Bool
exprIsCnf (Expr_And es) = and $ map exprIsClause es
exprIsCnf expr = exprIsClause expr

-- TODO: Remove?
--exprIsClauseStrict :: Expr -> Bool
--exprIsClauseStrict (Expr_Or es) = and $ map exprIsLit es
--exprIsClauseStrict _ = False
--
--exprIsCnfStrict :: Expr -> Bool
--exprIsCnfStrict (Expr_And es) = and $ map exprIsClauseStrict es
--exprIsCnfStrict _ = False

-- TODO: Fix: Counts duplicate variables multiple times.
exprCntVars :: Expr -> Int
exprCntVars (Expr_Var _) = 1
exprCntVars (Expr_Not e) = exprCntVars e
exprCntVars (Expr_And es) = sum $ map exprCntVars es
exprCntVars (Expr_Or es) = sum $ map exprCntVars es

exprFlatten :: Expr -> Expr
exprFlatten (Expr_And es) = Expr_And $ go es
  where
    go [] = []
    go ((Expr_And xs):rest) = map exprFlatten xs ++ go rest
    go (x:rest) = exprFlatten x : go rest
exprFlatten (Expr_Or es) = Expr_Or $ go es
  where
    go [] = []
    go ((Expr_Or xs):rest) = map exprFlatten xs ++ go rest
    go (x:rest) = exprFlatten x : go rest
exprFlatten e = e

cnfCntClauses :: Expr -> Int
cnfCntClauses (Expr_Var _) = 1
cnfCntClauses (Expr_Not _) = 1
cnfCntClauses (Expr_Or _) = 1
cnfCntClauses (Expr_And es) = length es

litToStr :: Expr -> Maybe String
litToStr (Expr_Not var) = consMaybe (Just '-') (litToStr var)
litToStr (Expr_Var var) = Just var
litToStr _ = Nothing

clauseToLine :: Expr -> Maybe String
clauseToLine (Expr_Not var) = litToStr (Expr_Not var) `appendMaybe` Just " 0"
clauseToLine (Expr_Var var) = litToStr (Expr_Var var) `appendMaybe` Just " 0"
clauseToLine (Expr_Or lits) =
  case mapM litToStr lits of
    Nothing -> Nothing
    Just lit_strs -> Just $ (intercalate " " lit_strs) ++ " 0"
clauseToLine _ = Nothing

-- Does not contain trailing newline.
dimacsHeaderLine :: Int -> Int -> String
dimacsHeaderLine n m = "p cnf " ++ show n ++ " " ++ show m

cnfToDimacs' :: Expr -> Maybe [String]
cnfToDimacs' expr
  | exprIsClause expr = case clauseToLine expr of
      Nothing -> Nothing
      Just line -> Just [line]
  | otherwise = case expr of
      (Expr_And clauses) -> mapM clauseToLine clauses
      _                  -> Nothing

cnfToDimacs :: Expr -> Either String String
cnfToDimacs expr
  | not $ exprIsCnf expr = Left "error: cnfToDimacs: input expression not in CNF"
  | isNothing clause_lines = Left "error: cnfToDimacs: failed to convert clauses to lines"
  | otherwise = Right $ unlines (header_line : fromJust clause_lines)
  where header_line = dimacsHeaderLine (exprCntVars expr) (cnfCntClauses expr)
        clause_lines = cnfToDimacs' expr
