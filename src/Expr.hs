module Expr where

import Util

data Expr
  = Expr_Var String
  | Expr_Not Expr
  | Expr_And [Expr]
  | Expr_Or [Expr]
  | Expr_Imply Expr Expr
  | Expr_Bicon Expr Expr

instance Eq Expr where
  (==) (Expr_Var v1) (Expr_Var v2) = (v1 == v2)
  (==) (Expr_Not e1) (Expr_Not e2) = (e1 == e2)
  (==) (Expr_And es1) (Expr_And es2) = and $ zipWith (==) es1 es2
  (==) (Expr_Or es1) (Expr_Or es2) = and $ zipWith (==) es1 es2
  (==) (Expr_Imply el1 er1) (Expr_Imply el2 er2) = (el1 == el2) && (er1 == er2)
  (==) (Expr_Bicon el1 er1) (Expr_Bicon el2 er2) = (el1 == el2) && (er1 == er2)
  (==) _ _ = False

instance Show Expr where
  show (Expr_Var var) = var
  show (Expr_Not expr) = if exprIsVar expr
                         then "-" ++ show expr
                         else "(- " ++ show expr ++ ")"
  show (Expr_And es) = "(*" ++ concat (map (\e -> " " ++ show e) es) ++ ")"
  show (Expr_Or es) = "(+" ++ concat (map (\e -> " " ++ show e) es) ++ ")"
  show (Expr_Imply el er) = "(-> " ++ show el ++ " " ++ show er ++ ")"
  show (Expr_Bicon el er) = "(<-> " ++ show el ++ " " ++ show er ++ ")"

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
        go (Expr_Imply el er) = go el ++ go er
        go (Expr_Bicon el er) = go el ++ go er

exprClauses :: Expr -> [Expr]
exprClauses (Expr_And es) = es
exprClauses expr = [expr]

-- NNF: Negation Normal Form
exprToNnf :: Expr -> Expr
exprToNnf (Expr_Imply _ _) = error "error: exprToNnf: implication not eliminated"
exprToNnf (Expr_Bicon _ _) = error "error: exprToNnf: biconditional not eliminated"
exprToNnf (Expr_Not (Expr_And es)) = Expr_Or $ map (exprToNnf . Expr_Not) es
exprToNnf (Expr_Not (Expr_Or es)) = Expr_And $ map (exprToNnf . Expr_Not) es
exprToNnf (Expr_Not (Expr_Not e)) = e
exprToNnf (Expr_And es) = Expr_And $ map exprToNnf es
exprToNnf (Expr_Or es) = Expr_Or $ map exprToNnf es
exprToNnf e = e

-- TODO: Should imply/bicon have left/right flattened instead of error?
exprFlatten :: Expr -> Expr
exprFlatten (Expr_Imply _ _) = error "error: exprFlatten: implication not eliminated"
exprFlatten (Expr_Bicon _ _) = error "error: exprFlatten: biconditional not eliminated"
exprFlatten (Expr_Not (Expr_Not e)) = exprFlatten e
exprFlatten (Expr_Not e) = Expr_Not $ exprFlatten e
exprFlatten (Expr_And [e]) = e
exprFlatten (Expr_And es) = Expr_And $ go es
  where go [] = []
        go ((Expr_And xs):rest) = go (map exprFlatten xs) ++ go rest
        go (x:rest) = exprFlatten x : go rest
exprFlatten (Expr_Or [e]) = e
exprFlatten (Expr_Or es) = Expr_Or $ go es
  where go [] = []
        go ((Expr_Or xs):rest) = go (map exprFlatten xs) ++ go rest
        go (x:rest) = exprFlatten x : go rest
exprFlatten e = e

-- Distribute ORs over ANDs of an expression. Expression must be flattened, and
-- implications and biconditionals must be eliminated (translated to NOT/AND/OR)
-- before this function is applied.
exprOrOverAnd :: Expr -> Expr
exprOrOverAnd (Expr_Imply _ _) = error "error: exprOrOverAnd: implication not eliminated"
exprOrOverAnd (Expr_Bicon _ _) = error "error: exprOrOverAnd: biconditional not eliminated"
exprOrOverAnd (Expr_Var var) = Expr_Var var
exprOrOverAnd (Expr_Not e) = Expr_Not $ exprOrOverAnd e
exprOrOverAnd (Expr_And es) = Expr_And $ map exprOrOverAnd es
exprOrOverAnd (Expr_Or es) = Expr_And $ go [] es
  where
    go :: [Expr] -> [Expr] -> [Expr]
    go taken [] = [Expr_Or $ reverse taken]
    go taken ((Expr_Var var):exprs) = go ((Expr_Var var):taken) exprs
    go taken ((Expr_Not e):exprs) = go ((Expr_Not e):taken) exprs
    go taken ((Expr_And and_es):exprs) = concat $ map (\e -> go (e:taken) exprs) and_es
    go _ ((Expr_Or _):_) = error "error: exprOrOverAnd: expression not flattened"
    go _ ((Expr_Imply _ _):_) = error "error: exprOrOverAnd: implication not eliminated"
    go _ ((Expr_Bicon _ _):_) = error "error: exprOrOverAnd: biconditional not eliminated"

exprElimBicon :: Expr -> Expr
exprElimBicon (Expr_Var var) = Expr_Var var
exprElimBicon (Expr_Not e) = Expr_Not $ exprElimBicon e
exprElimBicon (Expr_And es) = Expr_And $ map exprElimBicon es
exprElimBicon (Expr_Or es) = Expr_Or $ map exprElimBicon es
exprElimBicon (Expr_Imply el er) = Expr_Imply l r
  where (l, r) = (exprElimBicon el, exprElimBicon er)
exprElimBicon (Expr_Bicon el er) = Expr_And [Expr_Imply l r, Expr_Imply r l]
  where (l, r) = (exprElimBicon el, exprElimBicon er)

exprElimImply :: Expr -> Expr
exprElimImply (Expr_Not e) = Expr_Not $ exprElimImply e
exprElimImply (Expr_And es) = Expr_And $ map exprElimImply es
exprElimImply (Expr_Or es) = Expr_Or $ map exprElimImply es
exprElimImply (Expr_Imply el er) = Expr_Or [Expr_Not l, r]
  where (l, r) = (exprElimImply el, exprElimImply er)
exprElimImply (Expr_Bicon el er) =
  Expr_Bicon (exprElimImply el) (exprElimImply er)
exprElimImply e = e
