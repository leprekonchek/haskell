{-# OPTIONS_GHC -Wall #-}
module Lopukhina05 where

import Data.List
import Data.Char (digitToInt, intToDigit)

type Graph = [[Int]]

-- generate all combinations of n elements
allCombs :: Int -> [Char] -> [String]
allCombs 1 list = [[x] | x <- list]
allCombs n list = concatMap (\y -> [y++[x] | x <- list]) (allCombs (n-1) list)

-- Завдання 1 ------------------------------------------
lucky ::   Int ->  [String]
lucky n = filter isLucky (filter (\x -> head x /= '0') (allCombs (2*n) numbers))
    where numbers = ['0'..'9']

isLucky :: String -> Bool
isLucky num = let len = (length num) `div` 2
                  fnum = map digitToInt . fst $ splitAt len num
                  snum = map digitToInt . snd $ splitAt len num
              in sum fnum == sum snum

-- Завдання 2 -----------------------------------------  
queens :: Int -> [[Int]]
queens n = map reverse $ queens' n
    where queens' 0 = [[]]
          queens' k = [q:qs | qs <- queens' (k-1), q <- [1..n], isSafe q qs]
          isSafe try qs = not (try `elem` qs || oneDiagonal try qs)
          oneDiagonal try qs = any (\(col,q) -> abs (try - q) == col) $ zip [1..] qs

-- Завдання 3 -----------------------------------------
maxLen ::  [Int] -> Int
maxLen xs = length $ maxSeq xs
   
-- Завдання 4 -----------------------------------------
maxSeq ::  [Int] ->  [Int]
maxSeq xs = head $ sort $ allMaxSeq xs

-- Завдання 5 -----------------------------------------
allMaxSeq ::  [Int] -> [[Int]]
allMaxSeq xs = let seqs = allSeqs xs
                   maxi = maximum (map length seqs)
                in filter ((==maxi).length) seqs

allSeqs :: [Int] -> [[Int]]
allSeqs xs = filter (\sq -> (sort sq) == sq && uniq sq) (subsequences xs)
    where uniq [] = True
          uniq [_] = True
          uniq (s1:s2:sq) = s1 /= s2 && (uniq (s2:sq))

-- Завдання 6 -----------------------------------------
genExpr ::  Int -> Int -> [String]
genExpr a b = ans (show a) b

ans :: String -> Int -> [String]
ans a b = map rpnToNormal (map noSpaces (answer a b))

answer :: String -> Int -> [String]
answer x y = [eq| eq <- allVars x, (evalPol eq) == y]

allVars :: String -> [String]
allVars nums = map (intersperse ' ') [genEq n o | o <- (allCombs len "+-*"), n <- numbers]
    where numbers = filter noDuplicates (allCombs (length nums) nums)
          len = (length nums)-1

-- переводить з польскої форми до звичайного виразу
rpnToNormal :: String -> String
rpnToNormal rpn = concat $ rpnToNormal' rpn

rpnToNormal' :: String -> [String]
rpnToNormal' [] = [[]]
rpnToNormal' [r] = [[r]]
rpnToNormal' [r1,r2] = [[r1,r2]]
rpnToNormal' (r1:r2:r3:rpn) = (r1:r3:r2:[]):rpnToNorm rpn

rpnToNorm :: String -> [String]
rpnToNorm [] = [[]]
rpnToNorm [r] = [[r]]
rpnToNorm (r1:r2:rpn) = (r2:r1:[]):rpnToNorm rpn

-- створює вираз з цифр та операторів у польскій нотації
genEq :: String -> String -> String
genEq n o = concat $ generateEq n o 

generateEq :: String -> String -> [String]
generateEq [] o = [o]
generateEq [n] o = [[n],o]
generateEq (n1:n2:_) [] = [[n1,n2]]
generateEq (n1:n2:nums) (o:ops) = (n1:n2:o:[]):(generEq nums ops)

generEq :: String -> String -> [String]
generEq [] _ = [[]]
generEq _ [] = [[]]
generEq (n:nn) (o:oo) = (n:o:[]):[(genEq nn oo)]

-- прибираємо дублікати у списку
noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = and (noDuplicates' xs)
noDuplicates' :: Eq a => [a] -> [Bool]
noDuplicates' [] = [True]
noDuplicates' (x:xs) = (and [x /= y| y <- xs]):(noDuplicates' xs)

noSpaces :: String -> String
noSpaces str = filter (\x -> x /= ' ') str

-- польска нотація
evalPol :: String -> Int
evalPol = head . (foldl stEv []) . words
    where stEv :: [Int] -> String -> [Int]
          stEv (x:y:xs) "+" = (y + x):xs
          stEv (x:y:xs) "-" = (y - x):xs
          stEv (x:y:xs) "*" = (y * x):xs
          stEv xs str = (read str):xs
 
-- Завдання 7 -----------------------------------------
genExprBracket ::  Int -> Int -> [String]
genExprBracket = undefined

-- Завдання 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll g = filter (\s -> isTopolSort g s) (map stringToIntList (allSorts g))

allSorts :: Graph -> [String]
allSorts g = filter noDuplicates (allCombs (length g) nds)
    where nds = intListToString (nodes g)

intListToString :: [Int] -> String
intListToString list = map intToDigit list

stringToIntList :: String -> [Int]
stringToIntList str = map digitToInt str

isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort _ [] = True
isTopolSort g (x:xs) = (allElem (g !! x) xs) && (isTopolSort g xs)

allElem :: [Int] -> [Int] -> Bool
allElem [] _ = True
allElem (x:xs) ys = (elem x ys) && (allElem xs ys)

nodes :: Graph -> [Int]
nodes g = [0..(length g-1)]
--------------------------------------------
gr1 :: Graph 
gr1 = [[1,2,3], [], [3,4], [4],[]]