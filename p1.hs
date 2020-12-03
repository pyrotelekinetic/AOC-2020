correctSum :: Eq a => Num a => a -> a -> [(a, a)]
correctSum x y
  | x + y == 2020 = [(x, y)]
  | otherwise = []

everyPair :: [a] -> [(a, a)]
everyPair (x : []) = []
everyPair (x : xs) = map ((,) x) xs ++ everyPair xs

main = print $ everyPair [1, 2, 3, 4]
