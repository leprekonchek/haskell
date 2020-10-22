{-# OPTIONS_GHC -W #-}

-- OLD BILET 15
{- список цілих чисел, які мають рівно два дільники -}
twoDividers :: Int -> [Int]
twoDividers n = take n $ [x| x <- [1..], twoDivs x]

twoDivs :: Int -> Bool
twoDivs x = length ([z| d <- [2..x-1], let z = x `mod` d, z == 0]) == 2

-- OLD BILET 23
{- бере список списків й транспонує його -}
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:xss) = transpose xss
transpose ((x:xs):xss) = (x:[h|(h:_) <- xss]):transpose(xs:[t|(_:t) <- xss])

-- OLD BILET 22
-- не до конца правильно, кол-во нного элемента
-- в результате должно быть min/max кол-ва 
-- этого элемента в обоих множествах
{- бере мультимножини і повертає їх перетин -}
bagIntersect :: String -> String -> String
bagIntersect st1 st2 = set $ intersect st1 st2

intersect :: String -> String -> String
intersect [] _ = []
intersect (s1:ss1) s2
    | s1 `elem` s2 = s1 : intersect ss1 s2
    | otherwise = intersect ss1 s2

set :: String -> String
set [] = []
set (x:xs) = x:set (filter(/=x) xs)

-- OLD BILET 4
{- знаходить всі суффікси заданого списку -}
suffixs :: [a] -> [[a]]
suffixs xs = [drop i xs | i <- [0..length xs]] -- by prozenko

{- another implementation
suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix (x:xs) = (x:xs):(suffix xs)
-}

-- IPZ BILET 7
{- обробляє рядки файлу і виводить його в стандартний файл виведення stdout 
обробка файлу: замінює в рядку кожну max послідовність, складену з k>3 вживань
одного і того ж символу, послідовністю k(s), s - повторювальний символ
-}
seqSymb :: IO()
seqSymb = do putStr "Enter file name: "
             name <- getLine
             file <- readFile name
             res  <- perform file
             print res

perform :: String -> IO String
perform f = do return $ unlines ans
               where ans = map (\line -> concatMap max3 (group line)) (lines f)
                     max3 g = if (length g > 3) then ("("++(show$length g)++")"++[(head g)])
                              else g

{- zhulkevskiy code
seqSymb' :: IO()
seqSymb' = do
  putStrLn "Input name"
  name <- getLine
  content <- readFile name
  print $ unlines $ map 
  (\l-> concatMap 
        (\gr -> if (length gr >3) 
                then ("("++(show$ length gr)++")"++[(head gr)])  
				else gr) 
		        (group l)) (lines content)
-}
group :: (Eq a) => [a] -> [[a]]
group [] = [[]]
group ([l]) = [[l]]
group li@(l:ll:list)
    | l == ll = (takeWhile con li) : (group (dropWhile con li))
    | l /= ll = [l] : (group (ll:list))
    | otherwise = error "otherwise"
     where con x = x == l

-- IPZ BILET 8
{- вводить два рядки, якщо кожний містить ціле число (можливо зі знаком),
   то функція розпізнає цілі числа і виводить їх суму. інакше - error. 
   можуть бути проміжки перед і після числа
-}
sum2 :: IO()
sum2 = do putStr "Enter first line: "
          ll1 <- getLine
          putStr "Enter second line: "
          ll2 <- getLine
          let l2 = spaces ll2
              l1 = spaces ll1
           in if checkNums l1 && checkNums l2
              then let one = getNums l1
                       two = getNums l2
                    in print ((read $ one) + (read $ two))
              else error "not numbers"

spaces :: String -> String
spaces s = filter (/=' ') s

getNums :: String -> String
getNums str = filter isInt str

checkNums :: String -> Bool
checkNums n = if ('-' `elem` n) then c2 && c1 else c1
    where c1 = and $ map isInt n
          c2 = head n == '-'

isInt :: Char -> Bool
isInt c = c `elem` (['0'..'9'] ++ "-")

-- IPZ BILET 9, 13
{- будує по цілому додатньому числу m його представлення 
   у n-річній системі числення (n <= 16) -}
intToString :: Int -> Int -> String
intToString n m = undefined


countElems x st1 st2 = if ((length (filter (==x) st1)) > (length (filter (==x) st2)))
                       then filter (==x) st1 else filter (==x) st2

bagIntersect' :: String -> String -> String 
bagIntersect' [] _ = []
bagIntersect' st1@(x:xs) st2 = (countElems x st1 st2) ++ bagIntersect xs st2
