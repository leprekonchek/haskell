{-# OPTIONS_GHC -Wall #-}
module Lopukhina03 where

-- Код - рядок ?????? ??????? - ?????????? ???? '0' ..'9'
type Code = String

-- ???? ??? (Move) ???? ??????????? Move ?????????????? ?????? (Code) ? ??? ?????:  
--    ??????? "????" ? "????"  ? ??????????-????? ?? ????????? ?? ????-????? 
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Завдання 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (c:cd) (a:att)
    | c == a = 1 + exactMatches cd att
    | otherwise = exactMatches cd att

-- Завдання 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits cd = [count x cd| x <- ['0'..'9']] where
    count _ [] = 0
    count y (x:xs)
        | y == x = 1 + count y xs
        | otherwise = count y xs

-- Завдання 3 ----------------------------------------- 
matches :: Code -> Code -> Int
matches cd att = sum (takeMin (countDigits cd) (countDigits att)) where    
    takeMin _ [] = []
    takeMin [] _ = []
    takeMin (x:xs) (y:ys) = (min x y) : (takeMin xs ys)
 
-- Завдання 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att =
    let bulls = exactMatches cd att
        cows = (matches cd att) - bulls
    in Move att bulls cows

-- Завдання 5 -----------------------------------------   
isConsistent :: Move -> Code -> Bool
isConsistent (Move att bulls cows) cd = (Move att bulls cows) == (getMove cd att)

-- Завдання 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- Завдання 7 -----------------------------------------
allCodes :: Int -> [Code]
numbers :: [Char]
numbers = ['0'..'9']
allCodes 1 = [[num] | num <- numbers]
allCodes n = concatMap (\cdx -> [cdx++[num] | num <- numbers]) (allCodes (n-1))
   
-- Завдання 8 -----------------------------------------
solve :: Code -> [Move]
solve code = 
    let codes = allCodes (length code)    
    in generateSolution code codes where
    generateSolution _ [] = []
    generateSolution cd [att] = [getMove cd att]
    generateSolution cd (att:cdx) = 
        let mv = getMove cd att
        in mv : (generateSolution cd (filterCodes mv cdx))