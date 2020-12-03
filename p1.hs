correctSum :: Eq a => Num a => a -> a -> [(a, a)]
correctSum x y
  | x + y == 2020 = [(x, y)]
  | otherwise = []

everyPair :: [a] -> [(a, a)] --FixMe
everyPair (a : []) = []
everyPair (a : b : abs) = [(a, b)] ++ everyPair (a : abs) ++ everyPair (b : abs)

main = print $ everyPair [1, 2, 3, 4]
