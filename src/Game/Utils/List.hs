module Game.Utils.List(
 updateAt
,removeAt
,findIndexed
,safeIndex
) where

------------------------------------------------------------
-- List helpers
------------------------------------------------------------

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs
  | i < 0 || i >= length xs = xs
  | otherwise = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

-- | Safe list indexing — returns 'Nothing' for out-of-bounds indices.
safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs
  | n < 0 || n >= length xs = Nothing
  | otherwise               = Just (xs !! n)

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