module Game.Utils.List(
 updateAt
,removeAt
,findIndexed
) where

------------------------------------------------------------
-- List helpers
------------------------------------------------------------

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

-- | Return the first element (and its zero-based index) that
--   matches the predicate, or Nothing.
findIndexed :: (a -> Bool) -> [a] -> Maybe (Int, a)
findIndexed p = go 0
  where
    go _ [] = Nothing
    go i (x : rest)
      | p x       = Just (i, x)
      | otherwise = go (i + 1) rest