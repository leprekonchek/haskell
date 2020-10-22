-------kjkkkkkkkkkkkkkkkkkk
{-# OPTIONS_GHC -Wall #-}
module Rehearsal where
import Data.List
import Text.ParserCombinators.Parsec
-- 14 14 88888888888888888888888888888888
-- 14 14 88888888888888888888888888888888
-- 14 14 88888888888888888888888888888888
-- 14 14 88888888888888888888888888888888
-- 14 14 88888888888888888888888888888888
primeCnt::[Int]->Int
primeCnt (x:xs) | isPrime x = 1 + primeCnt xs
 | otherwise = primeCnt xs
primeCnt [] = 0

isPrime :: Int-> Bool
isPrime x = (not $ any (\y-> (mod x y) == 0) [2..x-1]) && (not $ x==1) 
-- 9
intToString :: Int-> Int-> String
intToString n m = let (xs,x) = until (cond1 n) (stepInt n) ([],m)
 in map (toChar) (x:xs)

cond1 :: Int -> ([Int],Int) -> Bool
cond1 n (_,x) = x < n

stepInt :: Int -> ([Int],Int)->([Int],Int)
stepInt n (xs,x) = let rest = mod x n
                       divRest = div x n
 in (rest:xs,divRest)
toChar :: Int -> Char 
toChar x = case x of
  0-> '0'
  1-> '1'
  2-> '2'
  3-> '3'
  4-> '4'
  5-> '5'
  6 -> '6'
  7 -> '7'
  8 -> '8'
  9 -> '9'
  10 -> 'a'
  11 -> 'b'
  12 -> 'c'
  13 -> 'd'
  14 -> 'e'
  15 -> 'f'  
  _ -> '0'
-- 26
seqWord :: IO()
seqWord = do
  putStrLn "Input name"
  name <- getLine
  content <- readFile name
  writeFile "lol.txt" $ unlines $ map (\l-> unwords $ nub $ words l) (lines content)

-- 13
stringToInt :: Integer -> String -> Maybe Integer
stringToInt n xs | canExecute n xs = Just $ foldl (stepToString n) 0 (zip (reverse xs) [0..])
 | otherwise = Nothing
stepToString :: Integer -> Integer -> (Char,Integer) -> Integer
stepToString n res c =  res+ (findInt $ fst c)*(n^(snd c))

canExecute::Integer->String->Bool
canExecute n xs = null [x|x<-xs,findInt x >= n ]

findInt ::  Char -> Integer 
findInt x = case x of
  '0'-> 0
  '1'-> 1
  '2'-> 2
  '3'-> 3
  '4'-> 4
  '5'-> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9'-> 9
  'a' -> 10
  'b' -> 11
  'c' -> 12
  'd' -> 13
  'e' -> 14
  'f' -> 15  
  _ -> 0

-- 7
seqSymb :: IO()
seqSymb = do
  putStrLn "Input name"
  name <- getLine
  content <- readFile name
  print $ unlines $ map (\l-> concatMap (\gr -> if (length gr >3) then ("("++(show$ length gr)++")"++[(head gr)])  else gr) (group l)) (lines content)
-- 25
seqDubl :: IO()
seqDubl = do
    putStrLn "Input name"
    name <- getLine
    content <- readFile name
    print $ unlines $ nub $ (lines content)

-- 17
lequation :: IO()
lequation = do
    putStrLn "Input equation"
    name <- getLine
    let x = parse equat "er" name
    print (findResult x)

equat :: Parser (Double,Double)
equat = do 
    x1<- integ <|> float
    _<- string "x+"
    x2<- integ <|> float
    _<-string "=0"
    return (x1,x2)

float :: Parser Double
float = do
    st1 <- try (count 3 digit) <|> try (count 2 digit) <|> (count 1 digit)
    _<-char '.'
    st2 <- try (count 4 digit) <|> try (count 3 digit) <|> try (count 2 digit) <|> (count 1 digit)
    return ((read (st1++"."++st2))::Double)
integ :: Parser Double
integ = do
    st1 <- try (count 3 digit) <|> try (count 2 digit) <|> try (count 1 digit)
    return ((read st1)::Double)

findResult :: (Eq a1, Show a1, Fractional a1) => Either a2 (a1, a1) -> [Char]
findResult (Right (0,0)) = "many"
findResult (Right (0,_)) ="no"  
findResult (Right (x1,x2)) = "x=" ++ show (-1*(x2/x1))
findResult (Left _) = "error"

-- 22
bagIntersect :: String -> String->String
bagIntersect s1 s2 = let b1 = makeBag s1 []
                         b2 = makeBag s2 []
  in sort $ concat [replicate (min (snd in1) (snd in2)) (fst in1)|in1<-b1,in2<-b2, (fst in1)==(fst in2) ]

makeBag :: String -> [(Char,Int)]-> [(Char,Int)]
makeBag (x:xs) bs= case applyBag x bs of
  Just bs1 -> makeBag xs bs1
  Nothing -> makeBag xs ((x,1):bs)
makeBag [] bs = bs

applyBag :: Char -> [(Char,Int)] -> Maybe [(Char,Int)]
applyBag x (b:bs) | fst b == x = Just ((x, snd b + 1):bs)
 | otherwise = case (applyBag x bs) of
  Just bs1 -> Just (b:bs1)
  Nothing -> Nothing
applyBag _ [] =Nothing  
