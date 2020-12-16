{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.Char
import Control.Monad.Except

type Policy = (Int, Int, Char)
type Error = String

newtype Parser a = MakeParser (String -> Maybe (a, String))
  deriving Functor

instance Monad Parser where
  return x = MakeParser $ \s -> Just (x, s)
  p >>= f = MakeParser $ \s ->
    case runParser p s of
      Just (x, s') -> runParser (f x) s'
      Nothing -> Nothing

instance Applicative Parser where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance Alternative Parser where
  empty = MakeParser $ const Nothing
  p <|> q = MakeParser $ \s ->
    case runParser p s of
      Nothing -> runParser q s
      Just x -> Just x

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MakeParser f) = f

passP :: Parser Char
passP = MakeParser $ \case
  c : cs -> Just (c, cs)
  _ -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MakeParser $ \case
  c : cs | p c -> Just (c, cs)
  _ -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

spaceP :: Parser String
spaceP = many $ satisfy isSpace

numP :: Parser Int
numP = do
  n <- some $ satisfy isDigit
  return $ read n

isWord :: Char -> Bool
isWord x
  | isDigit x = True
  | isAlpha x = True
  | otherwise = False

passwordP :: Parser String
passwordP = do
  p <- many $ satisfy isWord
  return p

policyP :: Parser Policy
policyP = do
  n1 <- numP
  charP '-'
  n2 <- numP
  spaceP
  c <- passP
  return $ (n1, n2, c)

lineP :: Parser (Policy, String)
lineP = do
  policy <- policyP
  charP ':'
  spaceP
  password <- passwordP
  charP '\n'
  return (policy, password)

finalP :: Parser [(Policy, String)]
finalP = many lineP

unwrap :: Maybe [(Policy, String)] -> Either Error [(Policy, String)]
unwrap = \case
  Nothing -> throwError "Parse Failure"
  Just x -> return x

finishedP :: Maybe (a, String) -> Maybe a
finishedP = \case
  Just (a, "") -> Just a
  _ -> Nothing

parse :: String -> Either Error [(Policy, String)]
parse = unwrap . finishedP . runParser finalP

elemIs :: Eq a => [a] -> a -> Int -> Bool
elemIs [] _ _ = False
elemIs (x : _) y 1 = x == y
elemIs (x : xs) y n = elemIs xs y (n - 1)

verify :: Policy -> String -> Bool
verify (n1, n2, c) p
  | length p < n2 || length p < n1 = False
  | otherwise = elemIs p c n1 /= elemIs p c n2

valids :: [(Policy, String)] -> Int
valids [] = 0
valids ((p, s) : xs)
  | verify p s = 1 + valids xs
  | otherwise = valids xs

main = do
  rawInput <- readFile "./2i.txt"
  let parsedInput = parse rawInput
  case parsedInput of
    Left error -> print error
    Right x -> print $ valids x
