{-# OPTIONS_GHC -Wall #-}
module Lopukhina02 where

-- Задача 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr = foldr (+) 0
  
-- Задача 2 ----------------------------------------- 
factorial :: Integer -> Integer
factorial x = foldl (*) 1 [1..x]

-- Задача 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) ys xs

-- Задача 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert = foldl insert [] where
insert :: [Integer] -> Integer -> [Integer]
insert [] x = [x]
insert (y:ys) x
    |x<y = x:y:ys
    |otherwise = y:insert ys x

-- Задача 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = ((f) x y) : map2 (f) xs ys

-- Задача 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum [(fromIntegral(m)^i)/fromIntegral(factorial i) | i <- [1..n]]

-- Задача 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Задача 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) [x*x|x<-[1..]]

-- Задача 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = isSubList 0 xs ys where
isSubList :: Int -> [Int] -> [Int] -> [Int]
isSubList _ [] [] = [0]
isSubList _ _ [] = []
isSubList _ [] ys = [0..(length ys)]
isSubList n xs yss@(_:ys)
    |xs == (take (length xs) yss) = n: isSubList (n+1) xs ys
    |otherwise = isSubList (n+1) xs ys