{-# OPTIONS_GHC -Wall #-}
module Lopukhina08 where

import Data.Tree

ot1, ot2, ot3 :: Tree Char
ot1 = Node 'A' []
ot2 = Node 'A' [Node 'A' []]
ot3 = Node 'A' [Node 'B' [], Node 'C' []]

b0, b1, b2, b3 :: BTree Char
b0 = BEmpty
b1 = BNode 'A' BEmpty BEmpty
b2 = BNode 'A' BEmpty (BNode 'B' BEmpty BEmpty )
b3 = BNode 'A' (BNode 'B' BEmpty BEmpty ) (BNode 'C' BEmpty BEmpty )


otx :: [Node Int]
otx = [ Node 1 [Node 2 [], 
                           Node 3 [Node 10 []] ] ,
            Node 4 [Node 5 [Node 8 []], 
                          Node 6 [Node 9 []],
                          Node 7 []] 
       ] 


bt ::  BTree Int
bt = BNode 1 (BNode 2 BEmpty
                             (BNode 3 (BNode 10 BEmpty BEmpty)
                                       BEmpty)
              ) 
              (BNode 4  (BNode 5 (BNode 8  BEmpty BEmpty)
                                 (BNode 6  (BNode 9 BEmpty BEmpty)
                                           (BNode 7 BEmpty BEmpty)
                                            )
                                  )
               BEmpty
               )

data BTree a = BEmpty | BNode a (BTree a) (BTree a)
               deriving (Show, Eq)

-- Задача 1 -----------------------------------------
dfsForest ::  Forest a -> [a]  
dfsForest forest = concatMap dfsTree forest

dfsTree :: Tree a -> [a]
dfsTree (Node n ts) = [n] ++ dfsForest ts

-- Задача 2 ----------------------------------------- 
bfsForest ::  Forest a -> [a]
bfsForest [] = []
bfsForest (Node n ns:ts) = n : (bfsForest (ts ++ ns))

-- Задача 3 -----------------------------------------
isInTree :: (Eq a) => Tree a -> Tree a -> Bool
isInTree _ (Node _ []) = False
isInTree tr (Node _ ts) 
    | any (\x -> x) (map (\t -> t == tr) ts) = True
    | otherwise = any (\x -> x) (map (\t -> isInTree tr t) ts)

-- Задача 4 -----------------------------------------
toBTree :: Forest a -> BTree a
toBTree [] = BEmpty
toBTree [(Node n [])] = BNode n BEmpty BEmpty
toBTree ((Node n ns):xs) = BNode n (toBTree ns) (toBTree xs)

-- Задача 5 -----------------------------------------
fromBTree :: BTree a -> Forest a  
fromBTree (BEmpty) = []
fromBTree (BNode n left right)  = [Node n (fromBTree left)] ++ fromBTree right

-- Задача 6 -----------------------------------------
isSearch :: (Ord a) => BTree a -> Bool
isSearch BEmpty = True
isSearch (BNode _ BEmpty BEmpty) = True
isSearch (BNode n1 BEmpty (BNode n2 bt1 bt2)) = (n2 > n1) && (isSearch (BNode n2 bt1 bt2))
isSearch (BNode n1 (BNode n2 bt1 bt2) BEmpty) = (n2 <= n1) && (isSearch (BNode n2 bt1 bt2))
isSearch (BNode n (BNode n1 bt11 bt21) (BNode n2 bt12 bt22)) = (n2 > n) && (n1 <= n) 
                                                                        && (isSearch (BNode n1 bt11 bt21)) 
                                                                        && (isSearch (BNode n2 bt12 bt22))

-- Задача 7  -----------------------------------------
elemSearch ::(Ord a) => BTree a -> a -> Bool
elemSearch BEmpty _ = False
elemSearch (BNode n tl tr) x
    | x == n = True
    | x < n = elemSearch tl x
    | x > n = elemSearch tr x
    | otherwise = False

-- Задача 8 ------------------------------------------
insSearch :: (Ord a) => BTree a -> a -> BTree a
insSearch BEmpty v = BNode v BEmpty BEmpty
insSearch (BNode n bt1 bt2) v
    | v <= n = BNode n (insSearch bt1 v) bt2
    | v > n  = BNode n bt1 (insSearch bt2 v)
    | otherwise = BNode v BEmpty BEmpty

-- Задача 9 ------------------------------------------
delSearch :: (Ord a) => BTree a -> a -> BTree a
delSearch t v
    | not(elemSearch t v) = t
    | otherwise = deleteSearch t v

deleteSearch :: (Ord a) => BTree a -> a -> BTree a
deleteSearch (BEmpty) _ = BEmpty
deleteSearch (BNode _ _ _) _ = BEmpty

-- Задача 10 -----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList xs = centerWalk (foldl (insSearch) BEmpty xs)

centerWalk :: (Ord a) => BTree a -> [a]
centerWalk BEmpty = []
centerWalk (BNode n bt1 bt2) = (centerWalk bt1) ++ [n] ++ (centerWalk bt2)