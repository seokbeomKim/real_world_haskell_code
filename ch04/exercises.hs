safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit x = Just (end x)
  where end [x] = []
        end (x:xs) = x : end xs

predicate x = x == 'c'

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith predicate list = [takeWhile predicate list] ++ (splitWith predicate remaining)
  where
    endOfList = dropWhile predicate list
    remaining
      | null endOfList = []
      | otherwise = tail endOfList
