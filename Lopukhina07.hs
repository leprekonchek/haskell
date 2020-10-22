{-# OPTIONS_GHC -Wall #-}
module Lopukhina07 where

data Stream a = Cons a (Stream a)

-- Екземпляр Show виводить перші 20 елементів, за якими розташовані крапки продовження
instance Show a => Show (Stream a) where
    show xs =  (foldl (++) "[" 
                  $ map (\x -> (show x) ++ ", ") $ take 20 $ streamToList xs
                ) ++ "..."

-- Задача 1 -----------------------------------------
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Задача 2 -----------------------------------------
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Задача 3 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat v = Cons v $ sRepeat v

sIterate :: (a -> a) -> a -> Stream a
sIterate f v = Cons v (sIterate f (f v))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons s s1) s2 = Cons s $ sInterleave s2 s1

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

-- Задача 4 -----------------------------------------
nats :: Stream Integer
nats = sIterate (+1) 0

-- Задача 5 -----------------------------------------
ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) ((1+) <$> ruler)

-- Задача 6 -----------------------------------------
rand :: Integer -> Stream Integer
rand r0 = Cons r0 (rand rN)
  where rN = ((1103515245 * r0) + 12345) `mod` 2147483648

-- Задача 7 -----------------------------------------
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Задача 8 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Задача 9 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)
         
instance Num a => Num (Matrix a) where
    (+) (M (x1,x2) (x3,x4)) (M (y1,y2)(y3,y4)) = M (x1+y1, x2+y2) 
                                                   (x3+y3, x4+y4)
    (*) (M (x1,x2) (x3,x4)) (M (y1,y2)(y3,y4)) = M (x1*y1 + x2*y3, x1*y2 + x2*y4) 
                                                   (x3*y1 + x4*y3, x3*y2 + x4*y4)
    negate (M (x1,x2) (x3,x4)) = M (negate x1, negate x2)
                                   (negate x3, negate x4)
    fromInteger x = M (fromInteger x, fromInteger 0) 
                      (fromInteger 0, fromInteger x)
    -- Реалізовувати не потрібно
    abs    = undefined
    signum = undefined

-- Задача 10 ----------------------------------------
fastFib :: Integer -> Integer
fastFib n = let (M (fibN,_) (_,_)) = (M (1,1) (1,0)) ^ (n) in fibN