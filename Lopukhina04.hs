{-# OPTIONS_GHC -Wall #-}
module Lopukhina04 where

import Data.Maybe (fromJust)

type Graph  = [[Int]]

-- сусіди вершини v в графі g
adj :: Graph -> Int -> [Int]
adj g v = g !! v

-- всі вершини графа g
nodes :: Graph -> [Int]
nodes g = [0..(length g-1)]

-- ребро належить графу
edgeIn :: Graph -> (Int,Int) -> Bool
edgeIn g (x,y) = y `elem` (adj g x)

-- всі ребра графу
edges :: Graph -> [(Int,Int)]
edges g = [(x,y)| x <- nodes g, y <- adj g x]

-- всі шляхи з однієї вершини
allAll :: Graph -> [[Int]]
allAll g = concat [allWays g v | v <- nodes g]

allWays :: Graph -> Int -> [[Int]]
allWays g v = concat (until isNull (step g) [[[v]]])

isNull :: ([[[Int]]]) -> Bool
isNull = null . head 

step :: Graph -> [[[Int]]] -> [[[Int]]]
step _ [] = []
step g wss@(wsn:_) = [t:w| w@(v:vs) <- wsn, v `notElem` vs, t <- g !! v] : wss

-- перевіряє чи ланцюг простий
easyPath :: [Int] -> Bool
easyPath p = and [easyPath' p x | x <- p]

easyPath' :: [Int] -> Int -> Bool
easyPath' path x = length (filter (/=x) path) == length path - 1

-- всі прості шляхи з однієї вершини
allEasyWays :: Graph -> Int -> [[Int]]
allEasyWays g v = filter (\path -> easyPath path) (allWays g v)

---------- Завдання 1 ------------
isGraph :: Graph -> Bool
isGraph g = and [oneWayEdge g (x,y)| (x,y) <- edges g]
-- check if there are dulicates for x -> y edge
oneWayEdge :: Graph -> (Int, Int) -> Bool
oneWayEdge g (x,y) = length(filter (/=y) (g !! x)) == length(g !! x) - 1

---------- Завдання 2 ------------
isTournament :: Graph -> Bool 
isTournament g = and [nodeT g x | x <- nodes g]

-- check if this node is connected with all others
nodeT :: Graph -> Int -> Bool
nodeT g n = and [edgeIn' g (n,x) | x <- nodes' g n]
-- all nodes except v
nodes' :: Graph -> Int -> [Int]
nodes' g v
    | v == 0 = [1..(length g-1)]
    | otherwise = [0..v-1] ++ [v+1..(length g-1)]
-- check if two nodes are connected by only one edge
edgeIn' :: Graph -> (Int,Int) -> Bool
edgeIn' g (x,y) 
    | elem y (g !! x) && elem x (g !! y) = False -- more than one edge
    | elem y (g !! x) || elem x (g !! y) = True
    | otherwise = False

---------- Завдання 3 ------------------------------------
isTransitive :: Graph -> Bool 
isTransitive g = and [existWay g (v,u1) (u2,w) | (v,u1) <- edges g, (u2,w) <- edges g, u1 == u2]

existWay :: Graph -> (Int,Int) -> (Int,Int) -> Bool
existWay g (v,_) (_,w) = (v,w) `elem` edges g

---------- Завдання 4 ------------------------------------
buildTransitive :: Graph -> Graph 
buildTransitive g
    | isTransitive g = g
    | otherwise = allEdges g

allEdges :: Graph -> [[Int]]
allEdges g = allCodes 2 g

allCodes :: Int -> Graph -> [[Int]]
allCodes 1 g = [[n] | n <- nodes g]
allCodes n g = concatMap (\e -> [e++[no] | no <- nodes g]) (allCodes (n-1) g)

---------- Завдання 5 ------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int] 
longWay g a b 
    | a == b = Nothing
    | pathesAB g a b == [] = Nothing
    | otherwise = Just (reverse $ snd $ maximum $ [(length x, x) | x <- pathesAB g a b])
                  where pathesAB gr start end = filter (\way -> (way !! 0) == end) (allEasyWays gr start)

---------- Завдання 6 ------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay g
    | cyclesWithAllNodes /= [] = Just (reverse $ head cyclesWithAllNodes)
    | otherwise = Nothing 
        where cyclesWithAllNodes = [cyc| cyc <- cycles g, containAll g cyc]
              containAll gr c = and [n `elem` c| n <- nodes gr]

cycles :: Graph -> [[Int]]
cycles g = filter (\way -> head way == last way) ([w| w <- allAll g])

---------- Завдання 7 ------------------------------------
isAcyclic :: Graph -> Bool 
isAcyclic g
    | (withoutLoops (cycles g)) == [] = True
    | otherwise = False
    where withoutLoops cycless = filter (\c -> (length c) /= 1) cycless

---------- Завдання 8 ------------------------------------
topolSort :: Graph -> Maybe [Int] 
topolSort g
    | not (isAcyclic g) = Nothing
    | otherwise = Just (dfs g [] [] [] (nodes g))

-- depth first search
dfs :: Graph -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
dfs _ _ stack _ [] = reverse stack
dfs g visited stack var (x:xs) 
   | x `elem` visited = dfs g visited (stack ++ var) [] xs
   | not $ null $ nextOne x (g!!x) = dfs g (x:visited) stack ([x] ++ var) ((nextOne x (g!!x)) ++ xs)
   | otherwise = dfs g (x:visited) (stack ++ [x] ++ var) [] xs
    where nextOne node ways = filter (\y -> y > node) ways

---------- Завдання 9------------------------------------
isTopolSort :: Graph -> [Int] -> Bool 
isTopolSort gr ts
    | fromJust (topolSort gr) == ts = True
    | otherwise = False

---------- Test's -------
gr1, gr2, gr3, gr4, gr5 :: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]
gr5 = [[3,4],[3],[4,7],[5,6,7],[6],[],[],[]]