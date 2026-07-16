{-# LANGUAGE TemplateHaskell #-}

module Util where

import qualified Data.Set as S (fromList, toList)
import Language.Haskell.TH (stringE)
import Language.Haskell.TH.Quote
  ( QuasiQuoter(..)
  , quoteDec, quoteExp, quotePat, quoteType)
import System.IO (Handle, hPutChar)

type ErrorMsg = String

hNewline :: Handle -> IO ()
hNewline handle = hPutChar handle '\n'

remDups :: Ord a => [a] -> [a]
remDups = S.toList . S.fromList

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe = liftA2 (:)

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appendMaybe = liftA2 (++)

padRight :: Int -> Char -> String -> String
padRight n c s = s ++ replicate (n - length s) c

-- TODO: Is this worth the TH dependency?
-- QuasiQuoter to apply at compile time.
padRightQQ :: Int -> Char -> QuasiQuoter
padRightQQ n c = QuasiQuoter
  { quoteExp = \s -> stringE (padRight n c s)
  , quotePat = error "error: padRightQQ: cannot be used in pattern"
  , quoteType = error "error: padRightQQ: cannot be used in type"
  , quoteDec = error "error: padRightQQ: cannot be used in declaration"
  }
