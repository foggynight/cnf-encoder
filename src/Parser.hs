module Parser where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)

import Expr
import Util

-- Lexer -----------------------------------------------------------------------

data Token
  = Tok_Err Char
  | Tok_Var String
  | Tok_Arrow_L
  | Tok_Arrow_R
  | Tok_Arrow_LR
  | Tok_Par_L
  | Tok_Par_R
  | Tok_Plus
  | Tok_Minus
  | Tok_Star
  deriving (Show)

isVarPrefix :: Char -> Bool
isVarPrefix c = (c == '_') || isAlphaNum c

nextToken :: String -> (Maybe Token, String)
nextToken [] = (Nothing, [])
nextToken (c:cs)
  | isSpace c            = (Nothing, cs)
  | isPrefixOf "(" str   = (Just Tok_Par_L, cs)
  | isPrefixOf ")" str   = (Just Tok_Par_R, cs)
  | isPrefixOf "<->" str = (Just Tok_Arrow_LR, drop 3 str)
  | isPrefixOf "<-" str  = (Just Tok_Arrow_L, drop 2 str)
  | isPrefixOf "->" str  = (Just Tok_Arrow_R, drop 2 str)
  | isPrefixOf "-" str   = (Just Tok_Minus, cs)
  | isPrefixOf "+" str   = (Just Tok_Plus, cs)
  | isPrefixOf "*" str   = (Just Tok_Star, cs)
  | isVarPrefix c        = let var = takeWhile isVarPrefix str
                           in (Just $ Tok_Var var, drop (length var) str)
  | otherwise            = (Just $ Tok_Err c, cs)
  where str = (c:cs)

lexStr :: String -> [Token]
lexStr [] = []
lexStr str =
  let (mb_tok, rest) = nextToken str
  in case mb_tok of
    Nothing  -> lexStr rest
    Just tok -> tok : lexStr rest

-- Parser ----------------------------------------------------------------------
-- TODO: Rewrite with a different way of handling operator precedence and
-- associativity. Recursive descent with loop for left-associative.

type Parser = [Token] -> Either ErrorMsg (Expr, [Token])

parseVar :: Parser
parseVar [] = Left "parseVar: no tokens to parse"
parseVar (Tok_Par_L:ts1) =
  case parseExpr ts1 of
    Left msg                      -> Left msg
    Right (expr, (Tok_Par_R:ts2)) -> Right (expr, ts2)
    Right (_, _)                  -> Left "parseVar: missing right parenthesis"
parseVar ((Tok_Var var):toks_rest) = Right (Expr_Var var, toks_rest)
parseVar (tok:_) = Left $ "parseVar: invalid token: " ++ show tok

parseLit :: Parser
parseLit (Tok_Minus:ts1) =
  case parseLit ts1 of
    Left msg         -> Left msg
    Right (lit, ts2) -> Right (Expr_Not lit, ts2)
parseLit toks = parseVar toks

parseConj :: Parser
parseConj tokens =
  case parseLit tokens of
    Left msg                   -> Left msg
    Right (lit, [])            -> Right (lit, [])
    Right (lit, (Tok_Star:ts)) -> go lit ts
    Right (lit, ts1)           -> case go lit ts1 of
                                    Left _ -> Right (lit, ts1)
                                    Right (expr, ts2) -> Right (expr, ts2)
  where
    go lit terms =
      case parseConj terms of
        Left msg                      -> Left msg
        Right ((Expr_And and_es), ts) -> Right (Expr_And (lit : and_es), ts)
        Right (other_term, ts)        -> Right (Expr_And [lit, other_term], ts)

parseDisj :: Parser
parseDisj tokens =
  case parseConj tokens of
    Left msg                    -> Left msg
    Right (conj, [])            -> Right (conj, [])
    Right (conj, (Tok_Plus:ts)) -> go conj ts
    Right (conj, ts)            -> Right (conj, ts)
  where
    go conj toks =
      case parseDisj toks of
        Left msg                    -> Left msg
        Right ((Expr_Or or_es), ts) -> Right (Expr_Or (conj : or_es), ts)
        Right (other_expr, ts)      -> Right (Expr_Or [conj, other_expr], ts)

-- TODO: Arrows currently right-associative, switch to left-associative.
parseExpr :: Parser
parseExpr tokens =
  case parseDisj tokens of
    Left msg                        -> Left msg
    Right (expr, [])                -> Right (expr, [])
    Right (expr, (Tok_Arrow_L:ts))  -> go_imply True expr ts
    Right (expr, (Tok_Arrow_R:ts))  -> go_imply False expr ts
    Right (expr, (Tok_Arrow_LR:ts)) -> go_bicon expr ts
    Right (expr, ts)                -> Right (expr, ts)
  where
    go_imply left expr toks =
      case parseExpr toks of
        Left msg          -> Left msg
        Right (other, ts) -> Right $ if left
                                     then (Expr_Imply other expr, ts)
                                     else (Expr_Imply expr other, ts)
    go_bicon expr toks =
      case parseExpr toks of
        Left msg          -> Left msg
        Right (other, ts) -> Right (Expr_Bicon expr other, ts)

parse :: String -> Either ErrorMsg Expr
parse str =
  let toks = lexStr str in
  case parseExpr toks of
    Left msg         -> Left msg
    Right (expr, []) -> Right expr
    Right (_, ts)    -> Left $ "parse: trailing tokens: " ++ show ts
