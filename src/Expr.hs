module Expr where

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

-- Note: `remDups` also sorts list.
exprVars :: Expr -> [String]
exprVars expr = remDups $ go expr
  where go (Expr_Var var) = [var]
        go (Expr_Not e) = go e
        go (Expr_And es) = concat $ map go es
        go (Expr_Or es) = concat $ map go es

exprClauses :: Expr -> [Expr]
exprClauses (Expr_And es) = es
exprClauses expr = [expr]

exprFlatten :: Expr -> Expr
exprFlatten (Expr_And es) = Expr_And $ go es
  where
    go [] = []
    go ((Expr_And xs):rest) = go (map exprFlatten xs) ++ go rest
    go (x:rest) = exprFlatten x : go rest
exprFlatten (Expr_Or es) = Expr_Or $ go es
  where
    go [] = []
    go ((Expr_Or xs):rest) = go (map exprFlatten xs) ++ go rest
    go (x:rest) = exprFlatten x : go rest
exprFlatten e = e

exprDeMorgans :: Expr -> Expr
exprDeMorgans (Expr_Not (Expr_And es)) = Expr_Or $ map (exprDeMorgans . Expr_Not) es
exprDeMorgans (Expr_Not (Expr_Or es)) = Expr_And $ map (exprDeMorgans . Expr_Not) es
exprDeMorgans e = e
