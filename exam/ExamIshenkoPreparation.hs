{-# OPTIONS_GHC -Wall #-}
module ExamPreparation where
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.List
-- билет 3 : просит ввести файл и дальше его обрабатывает его строчки и выводи результат - undone !
-- обработка : удаляет строчки в которых только пробелы или табуляция, а в остальных удаляет табуляцию или промежутки до 1 пробела

-- чтение файла
catSpaces :: IO()
catSpaces = do 
    f <- getLine
    content <- readFile f
    putStrLn $ process content

process :: String -> String
process s = let srs = lines s
             in unlines $ map repl $ filter (not . (all isSpace)) srs

repl :: String -> String
repl [] = []
repl (x:xs) 
    | not $ isSpace x = x : (repl xs)
    | otherwise = ' ' : repl(dropWhileM isSpace xs)

dropWhileM :: (Char -> Bool) -> String -> String
dropWhileM _ [] = []
dropWhileM f s@(x:xs) 
    | f x = dropWhileM f xs
    | otherwise = s

--билет 8 - done !
--sum2 проссумировать 2 числа которые вводят с клавиатуры, нужно распознать 2 целых числа и вывести их сумму, в других случаях вывести ошибку
sum2' :: IO()
sum2' = do
	a <- getLine
	b <- getLine
	putStrLn $ evalSum a b

evalSum :: String -> String -> String
evalSum a b = case (takeInt a, takeInt b) of
	(Just x, Just y) -> show(x+y)
	(_, _) -> "error"
	(a, _) -> "error"
	(_, b) -> "error"

takeInt :: String -> Maybe Int
takeInt sm = let (_, sm1)   = span (==' ') sm
                 (num, sm2) = span (`elem` "0123456789") sm1
                 (_, sm3)   = span (==' ') sm2
              in if null sm3 then Just (read num) else Nothing


--билет 14 : merge files -- нужно решить проблему с считыванием файлов


main :: IO()
main = do
	file1 <- getLine
	file2 <- getLine
	file3 <- getLine
	data1 <- readFile file1
	data2 <- readFile file2
	data3 <- readFile file3 
	writeFile "output" (data1 ++ data2 ++ data3) 

-- билет 11 
minGood :: Integer -> Integer
minGood n = head [ x | x <- [1..], (dividesByAll x n) == [1..n]]

dividesByAll :: Integer -> Integer -> [Integer]
dividesByAll m n = [ y | y <- [1..n], m mod y == 0]


transposeQ :: [[Int]] -> [[Int]]
transposeQ []             = []
transposeQ ([]   : xss)   = transposeQ xss
transposeQ ((x:xs): xss) = (x : [y | (y:_) <- xss]) : transposeQ (xs : [ys | (_:ys) <- xss])

suffix :: [String] -> [[String]]
suffix [] = []
suffix (x:xs)
    | length x == 1 = [[x]] ++ suffix xs
    | otherwise = [last x : [], tail x] : suffix xs

-- sortDividers :: [Integer] -> [Integer]
-- sortDividers xs = [snd r | r <- sort dividers]
--     where dividers = [(sum [if x `mod` y == 0 then 1 else 0 | y <- [1..x-1]] - 1, x )| x <- xs]

countSymbols :: Char -> String -> Integer
countSymbols c xs = sum [if c == x then 1 else 0 | x <- xs]

bagSubbag :: String -> String -> Bool
bagSubbag st1 st2 = all (==True) [(countSymbols s1 st2) >= (countSymbols s1 st1) | s1 <- st1]

suffixs :: String -> [String]
suffixs (x:xs) = [(x : []) ++ xs] ++ suffixs xs
suffixs [] = [[]]



-- Variant 7 - FAILED - can't replace substring with a custom string
subsets :: String -> [String]
subsets []  = [[]]
subsets (x:xs) = map (x:) (subsets xs) ++ subsets xs

replaceSeq :: String -> String
replaceSeq xs = toRep
    where toRep = snd $ maximum $ [(length x, x) | x <- subsets xs, length x >= 3, all (==head x) x]

seqSymb :: IO()
seqSymb = do name <- getLine
             handler <- readFile name
            --  let l = lines handler
             putStrLn handler

-- Variant 14 - DONE
-- primeCnt :: [Integer] -> [Integer]
-- primeCnt xs = [snd x | x <- dividers, fst x == 0]
--     where dividers = [(sum [if x `mod` y == 0 then 1 else 0 | y <- [1..x-1]] - 1, x )| x <- xs]

isPrime :: Int -> Int -> Bool
isPrime x n
    | x == 1 = False
    | x == 2 = True
    | n == (x-1) = if (x `mod` n /= 0) then True else False
    | otherwise =  if (x `mod` n /= 0) then isPrime x (n+1) else False

primeCntBetter :: [Int] -> [Int]
primeCntBetter xs = [x | x <- xs, isPrime x 2]

-- Variant 17
solveEq :: [String] -> String
solveEq xs
    | length xs /= 2 = "error"
    | isNumeric (head xs) == False || isNumeric (last xs) == False = "error"
    | x == 0 = "no"
    | otherwise = "x=" ++ (show ((-y) / x))
    where x = read $ head xs :: Float
          y = read $ last xs :: Float

isNumeric :: String -> Bool
isNumeric ""  = False
isNumeric "." = False
isNumeric xs  =
    case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

lequation :: IO()
lequation = do line <- getLine
               let nums = solveEq (words line)
               putStrLn nums

-- Variant Unknown - DONE
converToN :: Int -> Int -> [Int]
converToN 0 _ = []
converToN x n = (converToN (x `div` n) n) ++ [x `mod` n]

simplify :: Int -> String
simplify x
    | x == 10 = "a"
    | x == 11 = "b"
    | x == 12 = "c"
    | x == 13 = "d"
    | x == 14 = "e"
    | x == 15 = "f"
    | otherwise = show x

intToString :: Int -> Int -> String
intToString n x
    | n > 16 = "not supported"
    | otherwise = concat [simplify num | num <- converToN x n]


-- Variant 8 - DONE
sum2 :: IO()
sum2 = do a1 <- getLine
          a2 <- getLine
          putStrLn (show ((read a1 :: Float) + (read a2 :: Float))) `onException` putStrLn "error"

-- Variant 12 - DONE
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

genFibsList :: Integer -> [Integer] -> [Integer]
genFibsList n (x:xs)
    | x < n = [x] ++ genFibsList n xs
    | otherwise = []
genFibsList _ [] = []

sumFib :: Integer -> Integer
sumFib n = sum [x | x <- genFibsList n fibs, even x]

-- Variant Unknown 
isBalanced :: String -> Bool
isBalanced = bal (-1) 0
    where bal :: Int -> Int -> String -> Bool
          bal _ 0 [] = True
          bal _ _ [] = False
          bal _ (-1) _ = False
          bal i n ('(':bs) = bal (i+1) (n+1) bs
          bal i n (')':bs) = bal (i+1) (n-1) bs
          bal _ _ _ = error "err"




