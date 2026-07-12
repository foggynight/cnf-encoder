module Util where

consMaybe :: Maybe a -> Maybe [a] -> Maybe [a]
consMaybe = liftA2 (:)

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
appendMaybe = liftA2 (++)
