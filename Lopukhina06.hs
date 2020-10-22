{-# OPTIONS_GHC -Wall #-}
module Lopukhina06 where

import Data.List

newtype Poly a = P [a]

-- Задача 1 -----------------------------------------
x :: Num a => Poly a
x = P [0,1]

-- Задача 2 ----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (P p1) == (P p2) = zeros p1 == zeros p2
        where zeros = dropWhile (==0) . reverse
 
-- Задача 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
   show (P a)
    | filter (/=0) a == [] = show (0::Int)
    | otherwise = intercalate " + " . showOne . reverse $ zip [(0::Int)..] a
                    where showOne [] = []
                          showOne ((i,coef):xs)
                             | coef == 0 = showOne xs
                             | i == 0 && coef == 1 = "1" : showOne xs
                             | i == 0 && coef == -1 = "-1" : showOne xs
                             | i == 0 = zerone coef : showOne xs
                             | i == 1 = (zerone coef ++ "x") : showOne xs
                             | otherwise = (zerone coef ++ "x^" ++ show i) : showOne xs
                          zerone 0 = ""
                          zerone 1 = ""
                          zerone (-1) = "-"
                          zerone y = show y

-- Задача 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P $ add a b
    where add :: Num a => [a] -> [a] -> [a]
          add [] p2 = p2
          add p1 [] = p1
          add (p:p1) (pp:p2) = (p + pp) : add p1 p2 

-- Задача 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = foldl (+) (P [0]) (map (\xx -> multOne xx b) (degrees a))
        where degrees xs = zip [(0::Int)..] xs
              riseDegree xs deg
                  | deg == 0 = xs
                  | otherwise = riseDegree (0:xs) (deg-1)
              multOne (deg, coef) poly = P $ riseDegree (map (\xx -> xx*coef) poly) deg

-- Задача 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P (map negate a)
    fromInteger a = P [fromInteger a]
    -- Розумних означень не існує 
    abs    = undefined
    signum = undefined

-- Задача 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P a) xx = sum $ map (\(pow,coeff) -> coeff * (xx^pow)) $ zip [(0::Int)..] a

-- Задача 8 -----------------------------------------
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv 0 f = f
    nderiv n f = deriv (nderiv (n-1) f)

-- Задача 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P (_:[])) = P []
    deriv (P (_:xs)) = x * deriv (P xs) + (P xs)



