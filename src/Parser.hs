module Parser where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)

import Expr
import Util

-- Lexer -----------------------------------------------------------------------

data Token
  = Tok_Err Char
  | Tok_Var String
--  | Tok_Arrow_L
--  | Tok_Arrow_R
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
  | isSpace c           = (Nothing, cs)
  | isPrefixOf "(" str  = (Just Tok_Par_L, cs)
  | isPrefixOf ")" str  = (Just Tok_Par_R, cs)
  -- | isPrefixOf "<-" str = (Just Tok_Arrow_L, drop 2 str)
  -- | isPrefixOf "->" str = (Just Tok_Arrow_R, drop 2 str)
  | isPrefixOf "-" str  = (Just Tok_Minus, cs)
  | isPrefixOf "+" str  = (Just Tok_Plus, cs)
  | isPrefixOf "*" str  = (Just Tok_Star, cs)
  | isVarPrefix c       = let var = takeWhile isVarPrefix str
                          in (Just $ Tok_Var var, drop (length var) str)
  | otherwise           = (Just $ Tok_Err c, cs)
  where str = (c:cs)

lexStr :: String -> [Token]
lexStr [] = []
lexStr str =
  let (mb_tok, rest) = nextToken str
  in case mb_tok of
    Nothing  -> lexStr rest
    Just tok -> tok : lexStr rest

-- Parser ----------------------------------------------------------------------

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

parseTerm :: Parser
parseTerm toks =
  case parseLit toks of
    Left msg                   -> Left msg
    Right (lit, [])            -> Right (lit, [])
    Right (lit, (Tok_Star:ts)) -> go lit ts
    Right (lit, ts1)           -> case go lit ts1 of
                                    Left _ -> Right (lit, ts1)
                                    Right (expr, ts2) -> Right (expr, ts2)
  where
    go lit terms =
      case parseTerm terms of
        Left msg                      -> Left msg
        Right ((Expr_And and_es), ts) -> Right (Expr_And (lit : and_es), ts)
        Right (other_term, ts)        -> Right (Expr_And [lit, other_term], ts)

parseExpr :: Parser
parseExpr tokens =
  case parseTerm tokens of
    Left msg                    -> Left msg
    Right (term, [])            -> Right (term, [])
    Right (term, (Tok_Plus:ts)) -> go term ts
    Right (term, ts)            -> Right (term, ts)
  where
    go term toks =
      case parseExpr toks of
        Left msg                    -> Left msg
        Right ((Expr_Or or_es), ts) -> Right (Expr_Or (term : or_es), ts)
        Right (other_expr, ts)      -> Right (Expr_Or [term, other_expr], ts)

parse :: String -> Either ErrorMsg Expr
parse str =
  let toks = lexStr str in
  case parseExpr toks of
    Left msg         -> Left msg
    Right (expr, []) -> Right expr
    Right (_, ts)    -> Left $ "parse: trailing tokens: " ++ show ts
