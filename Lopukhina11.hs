{-# OPTIONS_GHC -Wall #-}
module Lopukhina11 where

import Data.Maybe
import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (iD, nodes) env
    | lookup iD nodes == Nothing = iD == 1
    | lookUp index env = checkSat (right, nodes) env
    | otherwise = checkSat (left, nodes) env
     where (index, left, right) = lookUp iD nodes

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp item xs = fromJust (lookup item xs)

-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat (n, nodes)
    | n == 0 = []
    | n == 1 = [[]]
    | otherwise = satF ++ satT
     where (iD, left, right) = lookUp n nodes
           satF = map ((iD, False):) (sat (left, nodes))
           satT = map ((iD, True):) (sat (right, nodes))

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Bvalue b)) = Bvalue $ not b
simplify (Or (Bvalue a) (Bvalue b)) = Bvalue (a || b)
simplify (And (Bvalue a) (Bvalue b)) = Bvalue (a && b)
simplify e = e

-- Задача 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict (Not e) x v   = simplify $ Not (restrict e x v)
restrict (Or a b) x v  = simplify $ Or (restrict a x v) (restrict b x v)
restrict (And a b) x v = simplify $ And (restrict a x v) (restrict b x v)
restrict e@(Bvar bv) x v = if bv == x then Bvalue v else e
restrict e _ _ = e

-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється
-- точно один раз в списку індексів (Index); немає інших елементів
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' e _ []
  | e == Bvalue False = (0, [])
  | otherwise = (1, [])
buildBDD' e n (x:xs) = (n,(n,(x, lid, rid)) : lnodes ++ rnodes)
    where (lid, lnodes) = buildBDD' (restrict e x False) (2*n) xs
          (rid, rnodes) = buildBDD' (restrict e x True) (2*n+1) xs

-- Задача 6 -----------------------------------------
-- we edit the BDD builded tree, instead of building new one from scratch
-- that's why it doesn't fit indices in examples
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD e xs = orderedBDD (buildBDD e xs)

orderedBDD :: BDD -> BDD
orderedBDD = removeIsomorphic . removeDuplicates

-- the problem is that it should check all nodes, not one by one
removeDuplicates :: BDD -> BDD
removeDuplicates (nid, bddn) = (nid, check bddn bddn)

check :: [BDDNode] -> [BDDNode] -> [BDDNode]
check [] bddn = bddn
check [_] bddn = bddn
check ((m1,(i1,f1,t1)):n2@(m2,(i2,f2,t2)):xs) bddn
  | m1 /= m2 && i1 == i2 && f1 == f2 && t1 == t2 = check newone newone
  | otherwise = check (n2:xs) bddn
  where newone = changeReferences (removeNode bddn n2) m1 m2

removeIsomorphic :: BDD -> BDD
removeIsomorphic (nid, bddn) = (nid, isomorphic bddn bddn)

isomorphic :: [BDDNode] -> [BDDNode]-> [BDDNode]
isomorphic [] bddn = bddn
isomorphic (n@(m,(_,f,t)):xs) bddn
  | f == t = isomorphic newone newone
  | otherwise = isomorphic xs bddn
   where newone = changeReferences (removeNode bddn n) f m

removeNode :: [BDDNode] -> BDDNode -> [BDDNode] 
removeNode bddn node = filter (\bdn -> bdn /= node) bddn

changeReferences :: [BDDNode] -> NodeId -> NodeId -> [BDDNode]
changeReferences bddn new old = map change bddn
    where change (m,(i,f,t))
           | m == old = (new,(i,f,t)) -- that's not neeeded (?)
           | f == old = (m,(i,new,t))
           | t == old = (m,(i,f,new))
           | otherwise = (m,(i,f,t))

-- Задача 7 -----------------------------------------
fullBexp :: String -> Maybe BExp
-- <fullBexp> ::= <bexp> ‘eos’ 
fullBexp s = case bexp s of
    Just (bxp, _) -> Just(bxp)
    _ -> Nothing 

------- BExp = Bvalue Bool | Bvar Char 
------- Not BExp | And BExp BExp | Or BExp BExp

bexp, bcon, bdis :: String -> Maybe (BExp,String)
-- <bexp> ::= <bcon> <manyCon>  
bexp s = case bcon s of
    Just (bx, st) -> case manyCon (bx, st) of
                        Just(bxp, str) -> Just(bxp, str)
                        _ -> Nothing
    _ -> Nothing

-- <bcon> ::= <bdis> <manyDis> 
bcon s = case bdis s of
    Just (bd, st) -> case manyDis (bd,st) of
                        Just(bxp, str) -> Just(bxp, str)
                        _ -> Nothing
    _ -> Nothing

-- <bdis> ::=  <bsym> |'(' <bexp> ')' | '!' <bdis> | 'T' | 'F' 
bdis "" = Nothing
bdis (s:ss)
  | bsym s = Just (Bvar s, ss)
  | present = case bexp ss of
              Just (bxp, str) -> if head str == ')' then Just (bxp, tail str) else Nothing
              _ -> Nothing
  | s == '!' = case bdis ss of
               Just (bxp, str) -> Just (Not bxp, str)
               _ -> Nothing
  | s == 'T' = Just(Bvalue True, ss)
  | s == 'F' = Just(Bvalue False, ss)
  | otherwise = Nothing
  where present = s == '(' && elem ')' ss

bsym :: Char -> Bool
-- <bsym> ::= 'a'|...|'z' 
bsym = isLower

manyCon, manyDis :: (BExp,String) -> Maybe (BExp,String)
-- <manyCon> ::= '|' <bcon> <manyCon> | ""
manyCon (bx, "") = Just (bx, "")
manyCon (bx,(s:ss))
  | s == '|' = case bcon ss of 
               Just (bex, st) -> manyCon (Or bx bex, st)
               _ -> Just (bx, (s:ss))
  | otherwise = Just (bx, (s:ss))

-- <manyDis> ::= '&' <bdis> <manyDis> | ""
manyDis (bx, "") = Just (bx, "")
manyDis (bx,(s:ss))
  | s == '&' = case bdis ss of
               Just (bex, st) -> manyDis (And bx bex, st)
               _ -> Just (bx, (s:ss))
  | otherwise = Just (bx, (s:ss))

------------------------------------------------------
-- Приклади для тестування..
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])