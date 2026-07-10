module Parser where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)

import Expr

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
isVarPrefix c = c == '_' || isAlphaNum c

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
    Nothing -> lexStr rest
    Just tok -> tok : lexStr rest

-- Parser ----------------------------------------------------------------------

-- TODO: Allow "*" to be omitted.

parseVar :: [Token] -> (Expr, [Token])
parseVar toks =
  case toks of
    [] -> (Expr_Err "parseVar: no tokens to parse", toks)
    (Tok_Par_L:ts1) ->
      let (expr, ts2) = parseExpr ts1
      in case ts2 of
           (Tok_Par_R:ts3) -> (expr, ts3)
           _ -> (Expr_Err "parseVar: missing right parenthesis", ts2)
    ((Tok_Var var):toks_rest) -> (Expr_Var var, toks_rest)
    _ -> (Expr_Err "parseVar: invalid token", toks)

parseLit :: [Token] -> (Expr, [Token])
parseLit (Tok_Minus:ts1) =
  let (var, ts2) = parseVar ts1
  in (Expr_Not var, ts2)
parseLit toks = parseVar toks

parseFact :: [Token] -> (Expr, [Token])
parseFact toks = parseLit toks

parseTerm :: [Token] -> (Expr, [Token])
parseTerm toks =
  let (fact, toks_rest) = parseFact toks in
  case toks_rest of
    [] -> (fact, [])
    (Tok_Star:ts1) -> let (other_term, ts2) = parseTerm ts1
                      in (Expr_And fact other_term, ts2)
    _ -> (fact, toks_rest)

parseExpr :: [Token] -> (Expr, [Token])
parseExpr toks =
  let (term, toks_rest) = parseTerm toks in
  case toks_rest of
    [] -> (term, [])
    (Tok_Plus:ts1) -> let (other_expr, ts2) = parseExpr ts1
                      in (Expr_Or term other_expr, ts2)
    _ -> (term, toks_rest)
