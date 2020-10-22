{-# OPTIONS_GHC -Wall #-}
module Lopukhina09 where

import Data.List hiding (insert, partition)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Ord, Show)

-- given function
buildTree :: String -> SuffixTree
buildTree s = foldl (flip insert) (Node []) (zip (suffixes s) [0..length s-1])

-- Задача 1 ----------------------------------- ------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (x:xs) (y:ys) 
    | x == y = isPrefix xs ys
    | otherwise = False

-- Задача 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition s1 [] = ([], s1, [])
partition [] s2 = ([], [], s2)
partition s1@(x:xs) s2@(y:ys) | x /= y = ([], s1, s2)
                              | otherwise = ((x:pref), tail1, tail2)
                              where (pref, tail1, tail2) = partition xs ys

-- Задача 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes = init . scanr (:) []

-- Задача 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring s1 s2 = any (isPrefix s1) (suffixes s2)

-- Задача 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = [i| (tf,i) <- zip (map (isPrefix s1) (suffixes s2)) [0..], tf]

-- Задача 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf i) = [i]
getIndices (Node st) = sort $ concatMap (getIndices . snd) st

-- Задача 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf _)  = []
findSubstrings' s (Node st) | st == [] = []
                            | isPrefix s str = getIndices sfxt
                            | isPrefix str s = findSubstrings' (removeStr str s) sfxt
                            | otherwise = findSubstrings' s (Node $ tail st)
                             where str = fst $ head st
                                   sfxt = snd $ head st

removeStr :: String -> String -> String
removeStr subStr str = filter (not . (`elem` subStr)) str

-- Задача 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, i) (Node []) = Node [(s, Leaf i)]
insert (s, i) (Leaf ii) = Node [("", Leaf ii), (s, Leaf i)]
insert (s, i) (Node ((a, st):remain))
    | p == ""   = Node ((a, st):remain')
    | p == a    = Node ((a, insert (sminp, i) st):remain)
    | otherwise = Node ((p, Node [(aminp, st), (sminp, Leaf i)]):remain)
    where (p, sminp, aminp) = partition s a
          Node remain' = insert (s, i) (Node remain)

-- Задача 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring st = longest [x | x <- allSuff, length (findSubstrings' x st) >= 2]
                              where allSuff = getAll st
                                    longest str = snd $ maximum $ [(length s, s) | s <- str]

getAll :: SuffixTree -> [String]
getAll (Leaf _) = [[]]
getAll (Node []) = [[]]
getAll (Node ((str, sfxt):xs)) = (map (str++) (getAll sfxt)) ++ (getAll $ Node xs)

------------------------------------------------------
ss1 :: String
ss1 = "banana"

ss2 :: String
ss2  = "mississippi"

tt1 :: SuffixTree
tt1 = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

tt2 :: SuffixTree
tt2 = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]