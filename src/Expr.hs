module Expr where

data Expr
  = Expr_Var String
  | Expr_Not Expr
  | Expr_And Expr Expr
  | Expr_Or Expr Expr

instance Show Expr where
  show (Expr_Var var) = var
  show (Expr_Not expr) = "(- " ++ show expr ++ ")"
  show (Expr_And e1 e2) = "(* " ++ show e1 ++ " " ++ show e2 ++ ")"
  show (Expr_Or e1 e2) = "(+ " ++ show e1 ++ " " ++ show e2 ++ ")"
