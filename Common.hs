module Common where

import qualified Data.Maybe as Maybe

safeHead :: [a] -> Maybe a
safeHead = Maybe.listToMaybe

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . safeHead . reads

removeFirstLine :: String -> String
removeFirstLine = unlines . tail . lines

count :: Eq a => a -> [a] -> Int
count a = length . filter ((== a))

everything :: a -> Bool
everything _ = True
