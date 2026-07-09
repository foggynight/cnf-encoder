module Parser where

import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)

data Token
  = ERR Char
  | VAR String
  | ARROW_L
  | ARROW_R
  | PAR_L
  | PAR_R
  | PLUS
  | MINUS
  | STAR
  deriving (Show)

isVarPrefix :: Char -> Bool
isVarPrefix c = c == '_' || isAlphaNum c

nextToken :: String -> (Maybe Token, String)
nextToken [] = (Nothing, [])
nextToken (c:cs)
  | isSpace c          = (Nothing, cs)
  | isPrefixOf "(" str = (Just PAR_L, cs)
  | isPrefixOf ")" str = (Just PAR_R, cs)
  | isPrefixOf "<-" str = (Just ARROW_L, drop 2 str)
  | isPrefixOf "->" str = (Just ARROW_R, drop 2 str)
  | isPrefixOf "-" str = (Just MINUS, cs)
  | isPrefixOf "*" str = (Just STAR, cs)
  | isPrefixOf "+" str = (Just PLUS, cs)
  | isVarPrefix c      = let var = takeWhile isVarPrefix str
                         in (Just $ VAR var, drop (length var) str)
  | otherwise          = (Just $ ERR c, cs)
  where str = (c:cs)

lexStr :: String -> [Token]
lexStr [] = []
lexStr str =
  let (mb_tok, rest) = nextToken str
  in case mb_tok of
    Nothing -> lexStr rest
    Just tok -> tok : lexStr rest

--parse :: [Token] -> Expr
