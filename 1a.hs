correctSum :: Eq a => Num a => a -> a -> Bool
correctSum x y
  | x + y == 2020 = True
  | otherwise = False

everyPair :: [a] -> [(a, a)]
everyPair (x : []) = []
everyPair (x : xs) = map ((,) x) xs ++ everyPair xs

solve :: Eq a => Num a => [(a, a)] -> a
solve [] = 0
solve ((x, y) : xs)
  | correctSum x y = x * y
  | otherwise = solve xs

main = do
  rawInput <- readFile "./1i.txt"
  let parsedInput = (map read (words rawInput) :: [Int])
  print $ solve $ everyPair $ parsedInput
