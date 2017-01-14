module Util (dedup, pprint)
where

import Data.Maybe

import qualified Data.Map as Map
import Evaluate (Env)

dedup :: Eq a => [a] -> [a]
dedup []  = []
dedup [x] = [x]
dedup (x:y:xs) | x == y    = dedup (y:xs)
               | otherwise = x:y:dedup xs

initMaybe :: [a] -> Maybe [a]
initMaybe [] = Nothing
initMaybe as = Just $ (init.init) as

pprint :: Env -> String
pprint e = fromMaybe "" (initMaybe $ go $ Map.toList e)
  where go [] = ""
        go ((k, v):ts) = k ++ " -> " ++ show v ++ ", " ++ go ts
