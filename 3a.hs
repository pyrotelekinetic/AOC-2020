type Error = String
type Coord = (Int, Int)

isTree :: Char -> Bool
isTree '.' = False
isTree '#' = True

diag :: Coord -> [[a]] -> Maybe a
diag _ [] = Nothing
diag (0, 0) ((a : _) : _) = Just a
diag (0, y) ((a : as) : bs) = diag (0, y - 1) ((a : as) : bs)
diag (x, 0) ((a : as) : bs) = diag (x - 1, 0) (as : bs)
diag (x, y) ((a : as) : bs) = diag (x - 1, y - 1) (as : bs)

path :: Coord -> [[a]] -> Either Error [a]
path _ [] = Left "error:tm:"
path (x, y) ((a : as) : bs) = case diag (x, y) ((a : as) : bs) of
  Nothing -> Left "error"
  Just thing -> do
    otherThing <- path (x - 1, y - 1) (as : bs)
    Right $ thing : otherThing

main = do
  rawInput <- readFile "./3i.txt"
  let parsedInput = lines rawInput
  print $ path (3, 1) parsedInput
