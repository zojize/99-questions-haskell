-- https://wiki.haskell.org/99_questions

import Control.Arrow ((&&&))
import Data.List (group)

-- Problem 1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [x] = Just x
myLast (x : xs) = myLast xs

-- Problem 2

myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [_] = Nothing
myButLast [x, _] = Just x
myButLast (x : xs) = myButLast xs

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt [] _ = error "invalid index"
elementAt (x : xs) i
  | i < 0 = error "invalid index"
  | otherwise = elementAt xs (i - 1)

-- Problem 4

myLength :: [a] -> Int
myLength = foldr (const (+ 1)) 0

-- Problem 5

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome s = (head s == last s) && isPalindrome (tail $ init s)

-- Problem 7

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

-- Problem 8

compress :: Eq a => [a] -> [a]
compress = map head . group

-- Problem 9

pack :: Eq a => [a] -> [[a]]
pack = foldr f []
  where
    f x [] = [[x]]
    f x (y : ys) =
      if head y == x
        then (x : y) : ys
        else [x] : y : ys

-- Problem 10

encode :: Eq a => [a] -> [(Int, a)]
encode = map f . pack
  where
    f x = (length x, head x)

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (length &&& head) . group

encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = map ((,) <$> length <*> head) . group

-- Problem 11

data ListItem a
  = Single a
  | Multiple Int a
  deriving (Show)

encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map f . encode
  where
    f (1, x) = Single x
    f (n, x) = Multiple n x

-- Problem 12

decodeModified :: [ListItem a] -> [a]
decodeModified = concatMap f
  where
    f (Single x) = [x]
    f (Multiple n x) = replicate n x

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map f . encode
  where
    f (1, x) = Single x
    f (n, x) = Multiple n x
    encode = foldr encodeHelper []
    encodeHelper x [] = [(1, x)]
    encodeHelper x (t@(n, y) : ys) =
      if x == y
        then (n + 1, y) : ys
        else (1, x) : t : ys

-- Problem 14

dupli :: [a] -> [a]
dupli = concatMap $ replicate 2

-- Problem 15

repli :: [a] -> Int -> [a]
repli = flip $ concatMap . replicate

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (i, x) <- zip [1 ..] xs, (i `mod` n) /= 0]

-- Problem 17

split :: [a] -> Int -> ([a], [a])
split xs n = foldr f ([], []) $ zip [0 ..] xs
  where
    f (i, x) (left, right)
      | i < n = (x : left, right)
      | otherwise = (left, x : right)
