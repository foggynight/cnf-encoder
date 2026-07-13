module Util where

import Data.Set (fromList, toList)

remDups :: Ord a => [a] -> [a]
remDups = toList . fromList

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe = liftA2 (:)

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appendMaybe = liftA2 (++)
