--------------------------------------------------------
-- Tested
--------------------------------------------------------

import Data.List
{-
�������� ������� suffix, �� ��������� ������� �������� ������.
Test cases:
suffix "abc" == ["abc","bc","c",""]
suffix "" == [""]
-}

suffix :: [a] -> [[a]]
suffix [] = [[]]
suffix (x:xs) = (x:xs):(suffix xs)

{-
����� ����� ���������� �� ������������� �������. 
���������, "abac" - ������� �� �� ��� ������� 'a' � �� ������ ������� 'b' � 'c'.
�������� ������� bagIntersect st1 st2, ����� ���� ������������� st1 �� st2 � ������� �� �������.
������� ������������� X � Y ������ �� ��������, �� ������������ � X � Y ������� ���������
�������� � ��������, �� ��������� ������� ���� ��������� � X ��� Y.
Test cases:
bagIntersect "abbc" "abc" == "abc"
bagIntersect "abbc" "" == ""
bagIntersect "" "abbc" == ""
bagIntersect "abbc" "bccc" == "bc"
bagIntersect "abbckk" "bccckk" == "bckk"
bagIntersect "abbckk" "bccck" == "bck"
-}
bagIntersect :: String -> String -> String
bagIntersect _ [] = []
bagIntersect [] _ = []
bagIntersect (x:xs) ys | x `elem` ys = x:(bagIntersect xs (remove x ys))
                       | otherwise = (bagIntersect xs (remove x ys))

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove q (w:ws) | q == w = ws
                | otherwise = w:(remove q ws)

{-
���������� ������� primeNotDiv3LN :: Int -> [Int], ��� ������ �������� � ������ �����
������ �����, �� �� ������� �� 3 �� ������ �� N.
Test cases:
primeNotDiv3LN 0 == []
primeNotDiv3LN 3 == [2]
primeNotDiv3LN 4 == [2]
primeNotDiv3LN 17 == [2,4,8,10,14,16]
-}
primeNotDiv3LN :: Int -> [Int]
primeNotDiv3LN x = [t | t <- [1..x], t `mod` 3 /= 0, even t]

{-
������ ����� ���������� �� ������������� ������.
���������, "abac" - ������� �� �� ��� ������� 'a' � �� ������ ������� 'b' � 'c'.
�������� ������� bagDif st1 st2, ����� ���� ������������� st1 �� st2 � ������� �� ������.
г����� ������������ X � Y ������ �� ��������, �� ������������ � X �� ������ ������� ��� ��� � Y
������� ��������� �������� � ��������, �� ������� ���� ��������� � X ���� ������� ��������� � Y.
Test cases:
bagDif "abc" "" == "abc"
bagDif "" "abc" == ""
bagDif "aabbcc" "abc" == "abc"
bagDif "abc" "aabbcc" == ""
bagDif "abc" "abc" == ""
-}

bagDif :: String -> String -> String
bagDif x [] = x
bagDif [] _ = []
bagDif (x:xs) ys | a >= b = c ++ (bagDif d e) -- x count in X bigger than x count in Y
                 | otherwise = bagDif d e -- ignore x
 where a = count x (x:xs) -- count of repeated x in X
       b = count x ys -- count of repeated x in Y
       c = generate x (a-b) -- list of a-b x values
       d = removeAll x xs -- X without x values
       e = removeAll x ys -- Y without x values

removeAll :: Eq a => a -> [a] -> [a]
removeAll _ [] = []
removeAll x (y:ys) = [a | a <- (y:ys), a /= x]

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x (y:ys) | x == y = 1 + (count x ys)
               | otherwise = count x ys

generate :: a -> Int -> [a]
generate x n | n > 0 = x:(generate x (n-1))
             | otherwise = []

{-
������ ����� ���������� �� ������������� ������.
���������, "abac" - ������� �� �� ��� ������� 'a' � �� ������ ������� 'b' � 'c'.
�������� ������� bagSubbag st1 st2, ����� ������� True, ���� ������������� st1 ��������� ���������������� st2.
X - ��������������� Y, ���� ������ ������� X ����������� � Y �� ����� ������� ��� ��� � X.
Test cases:
bagSubbag "" "" == True
bagSubbag "" "abc" == True
bagSubbag "abc" "abc" == True
bagSubbag "abc" "abcd" == True
bagSubbag "abcc" "abcd" == False
bagSubbag "abcc" "abccd" == True
bagSubbag "acc" "abccd" == True
-}

bagSubbag:: String->String->Bool
bagSubbag [] _ = True
bagSubbag _ [] = False
bagSubbag (x:xs) ys 
 |contains = True && (bagSubbag xs (delElFirst x ys)) 
 |otherwise = False
 where contains = foldl (\acc y->if x==y then acc || True else acc) False ys

-- �������� ������� ��� bagSubbag
delElFirst::(Eq a) =>a-> [a] -> [a]
delElFirst _ [] = []
delElFirst el (l:list)
 |el==l = list
 |otherwise = l:(delElFirst el list)

{- or
bagSubbag :: String -> String -> Bool
bagSubbag [] [] = True
bagSubbag [] _ = True
bagSubbag (x:xs) ys = a <= b && (bagSubbag xs ys)
 where a = count x (x:xs) -- count of repeated x in X
       b = count x ys -- count of repeated x in Y
-}

{-
���������� ���� ��������� ��������, 
���� � ��'���� ����� - ��������� Nothing
Test cases:
sumFact [] == Nothing
sumFact [1] == Just 1
sumFact [3] == Just 6
sumFact [5] == Just 120
sumFact [5,1,3] == Just 127
sumFact [5,1,3,-3] == Nothing
sumFact [0,-3,-1,-3] == Nothing
sumFact [0,0,0,0] == Just 4
-}

sumFact :: [Int] -> Maybe Int
sumFact [] = Nothing
sumFact xs | (not $ check xs) = Nothing
           | otherwise = Just $ foldl1 (+) (map factorial xs)
 where
 check [] = True
 check (q:qs) = q >= 0 && check qs
 factorial a | a <= 0 = 1
             | otherwise = a * (factorial (a-1))

--------------------------------------------------------
-- IO
--------------------------------------------------------

getFileName :: String -> IO String
getFileName prompt = do
 putStr prompt
 getLine

{-
��������� ������ �����
Test cases:
checkBalance "(()())" == True 
checkBalance "()" == True
checkBalance "()()()" == True
checkBalance "((()))" == True
checkBalance "(2+2)*2-(3-3)" == True
checkBalance "" == True
checkBalance "(" == False
checkBalance "())" == False
checkBalance ")" == False
checkBalance ")(" == False
checkBalance "(asdw((asca)" == False
checkBalance "(Hello" == False
-}
balance :: IO()
balance = do
 from <- getFileName "From: "
 s <- readFile from
 case (checkBalance s) of
  True -> putStr "Balanced\n"
  False -> putStr "Unbalanced\n"

checkBalance :: String -> Bool
checkBalance [] = True
checkBalance xs = helper xs []
 where
 helper "" stack = (length stack) == 0
 helper (x:xs) stack | x == '(' = helper xs (x:stack) -- push
                     | x == ')' && (length stack) == 0 = False
                     | x == ')' = helper xs (tail stack) -- pop
                     | otherwise = helper xs stack -- ignore


--------------------------------------------------------
-- Not tested
--------------------------------------------------------

-- ³���������� ������ ����� �� �-�� �������
sortDividers::[Int] -> [Int]
sortDividers [] = []
sortDividers [x] = [x]
sortDividers (x:xs) = (sortDividers l) ++ m ++ (sortDividers r)
 where dc n = length $ filter dividor [ d | d <- [2..n]] -- dividers count
       l = [a | a <- xs, dc a < dc x]
       m = [a | a <- (x:xs), dc a == dc x]
       r = [a | a <- xs, dc a > dc x]
       dividor a = (a `mod` x == 0)


-- �������� �������, �� �������� ������ [�����, �-�� �������] 
dividers :: Int -> [Int]
dividers x = [x, length [t | t <- [1..x], x `mod` t == 0, t /= 1, t /= x]]

-- �������������� �������(����� �������� �� ���������)
transposes :: [[a]] -> [[a]]
transposes [] = []
transposes ([] : xss) = transposes xss
transposes ((x:xs):xss) = (x:[t | (t:_) <- xss]) : transposes (xs : [h | (_:h) <- xss])


-- ���� ������
listSum :: [Int] -> [Int] -> [Int]
listSum [] y = y 
listSum x [] = x
listSum x y = [(head x) + (head y)] ++ listSum (tail x) (tail y)


-- ̳����� ������ ����� �� �������
oddEven :: [Int] -> [Int] 
oddEven [x] = [x]
oddEven [x,y] = [y,x]
oddEven xs = [(head(tail xs)), (head xs)] ++ oddEven (tail(tail xs))


-- ������� ������ ������� ��������� �����
position:: Int->[Int]->Int
position el list = snd ( head ( filter (\q->(fst q)==el) qq))
 where qq=zip list [0..]


-- ������� ������ �����, ��� ���������
set :: [Int] -> [Int]
set [] = []
set (x:xs) = x :( set (delEl x xs))

delEl ::(Eq a) =>a-> [a] -> [a]
delEl x xs = [t | t <- xs, t /= x]

-- �������� ������ ��� ��������
unionL :: [Int] -> [Int] -> [Int]
unionL xs [] = xs
unionL [] xs = xs
unionL xs (y:ys) = (head xs): (unionL (delEl y xs) ys)


-- ���� �������� ������ ����� foldr
sumFr :: [Int] -> Int
sumFr xs = foldr (\ x acc  -> acc + x) 0 xs


-- �������� ����� foldl
factorialFoldl :: Int -> Int
factorialFoldl x
  | x> 0 = foldl(\ xs acc -> acc*xs) 1 [1..x] 
  |otherwise = -1

-- �������� �-�� �������, �������� �����
primeCnt :: [Int] -> Int
primeCnt xs = length [t | t <- xs, t > 0, last (dividers t) == 0 ]


{- NOT FINISHED
�������, ��� ������ string - ����� � ���� ������ ��������, 
� int - ������ ���� ������� �������� <= 16. ����� ����� 10 ������������ �� a �� f. 
��������� �� ����� � ��������� ������ Just Integer, ���� ���� ��������� ������ � 
���� ������ ��������, � Nothing, ���� �����������.
Test cases:
checkNumber "123" 0 == Nothing
checkNumber "q" 1 == Nothing
checkNumber "000" 1 == Nothing
checkNumber "000" 1 == Nothing
-}
checkNumber :: String -> Int -> Maybe Integer
checkNumber [] _ = Nothing
checkNumber ('-':xs) n = checkNumber xs n
checkNumber xs n | check xs n = Just (toDecimal xs n)
                 | otherwise = Nothing
 where
 check (q:qs) w = (ch q w) && (check qs w)
 ch c n | not $ (number c && letter c) = False
        | n < 1 || n > 16 = False
        | n == 1 = (num c) == 1
        | n > 1 && n <= 10 = ((num c) >= 0 && (num c) < 10)
        | n == 11 = sysMoreThan10ch c 'a'
        | n == 12 = sysMoreThan10ch c 'b'
        | n == 13 = sysMoreThan10ch c 'c'
        | n == 14 = sysMoreThan10ch c 'd'
        | n == 15 = sysMoreThan10ch c 'e'
        | n == 16 = sysMoreThan10ch c 'f'
 letter c = (c >= 'a' && c <= 'f')
 number c = (c >= '0' && c <= '9')
 num a | (number a) = read (a:[])::Int
       | otherwise = -1
 sysMoreThan10ch c boundary = ((num c) >= 0 && (num c) < 10) || (c >= 'a' && c <= boundary)
 
-- TODO: Write toDecimal s n function, which will convert the number s
-- from number with base n to number with base 10
toDecimal:: String -> Int -> Integer
toDecimal _ _ = 0