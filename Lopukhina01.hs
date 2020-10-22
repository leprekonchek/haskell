{-# OPTIONS_GHC -Wall #-}
module Lopukhina01 where

-- Задача 1 -----------------------------------------
factorial :: Integer -> Integer
factorial n = if n==0 then 1 else n*factorial (n-1) 

-- Задача 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum xs ys = if null xs then ys 
else if null ys then xs 
else (head xs + head ys) : listSum (tail xs) (tail ys)

-- Задача 3 ----------------------------------------- 
oddEven :: [Int] -> [Int] 
oddEven [] = []
oddEven [xs] = [xs]
oddEven (x1 : x2 : xs) = x2 : x1 : oddEven xs

-- Задача 4 -----------------------------------------
position ::  Int -> [Int] -> Int
position2 ::  Int -> [Int] -> Int

position n x = if (elem n x) == False then -1 else position2 n x
position2 _ [] = -1
position2 n (x:xs) = if n == x then 0 else 1 + position n xs
                     
-- Задача 5 -----------------------------------------
set :: [Int] -> [Int] 
set [] = []
set (x:xs) = x:set (filter(/=x) xs)

-- Задача 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs ++ ys)

-- Задача 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection xs ys = set([zs | zs <- xs, zs `elem` ys])

-- Задача 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = [factorial x | x <- [1..]]