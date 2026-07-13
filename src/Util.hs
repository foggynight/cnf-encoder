module Util where

import qualified Data.Set as S (fromList, toList)

remDups :: Ord a => [a] -> [a]
remDups = S.toList . S.fromList

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe = liftA2 (:)

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appendMaybe = liftA2 (++)
