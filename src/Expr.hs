module Expr where

data Expr = VAR String | NOT Expr | AND Expr Expr | OR Expr Expr
