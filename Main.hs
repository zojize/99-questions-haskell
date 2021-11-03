-- https://wiki.haskell.org/99_questions

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
        else [x] : ys
