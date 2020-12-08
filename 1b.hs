correctSum :: Eq a => Num a => a -> a -> a -> Bool
correctSum x y z
  | x + y + z == 2020 = True
  | otherwise = False

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (\y -> (x, y)) xs ++ pairs xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples (x : xs) = (map (\(y, z) -> (x, y, z)) $ pairs xs) ++ triples xs

solve :: Eq a => Num a => [(a, a, a)] -> a
solve [] = 0
solve ((x, y, z) : xs)
  | correctSum x y z = x * y * z
  | otherwise = solve xs

main = putStr $ unlines $ map show $ triples [1, 2, 3, 4, 5, 6]
--main = do
--  rawInput <- readFile "./1i.txt"
--  let parsedInput = (map read (words rawInput))
--  print $ solve $ everyTriple $ parsedInput
