module Expr where

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
