{-# OPTIONS_GHC -Wall #-}
module Lopukhina12 where

data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Змінна
         | Const Value     -- Константа
         | Op Exp Bop Exp  -- Операція
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le | Eql | And | Or
            deriving (Show, Eq)

data Stmt = Assign String Exp
          | Incr String
          | If Exp Stmt Stmt
          | While Exp Stmt       
          | For Stmt Exp Stmt Stmt
          | Block [(String,Type)] [Stmt]        
          deriving (Show, Eq)
data Type = It | Bt deriving (Show, Eq)
type Program = Stmt

type StateW = [(String, Value)]

type VarEnv  = [(String,Type)]

-- Задача 1 -----------------------------------------
getValue::  StateW -> String -> Value
getValue st idd = snd $ head $ filter (\x -> fst x == idd) st

updValue :: StateW -> String -> Value -> StateW
updValue st idd v = map (\x -> if fst x == idd then (idd,v) else x) st

-- Задача 2 ----------------------------------------- 
evExp :: StateW -> Exp -> Value
evExp st (Var x) = getValue st x
evExp _ (Const x) = x
evExp st (Op exp1 Plus exp2)  = op (evExp st exp1) (evExp st exp2) (+)
evExp st (Op exp1 Minus exp2) = op (evExp st exp1) (evExp st exp2) (-)
evExp st (Op exp1 Times exp2) = op (evExp st exp1) (evExp st exp2) (*)
evExp st (Op exp1 Div exp2)   = op (evExp st exp1) (evExp st exp2) (div)
evExp st (Op exp1 Gt exp2)  = ibop (evExp st exp1) (evExp st exp2) (>)
evExp st (Op exp1 Ge exp2)  = ibop (evExp st exp1) (evExp st exp2) (>=)
evExp st (Op exp1 Lt exp2)  = ibop (evExp st exp1) (evExp st exp2) (<)
evExp st (Op exp1 Le exp2)  = ibop (evExp st exp1) (evExp st exp2) (<=) 
evExp st (Op exp1 Eql exp2) = ibop (evExp st exp1) (evExp st exp2) (==)
evExp st (Op exp1 And exp2) = bbop (evExp st exp1) (evExp st exp2) (&&)
evExp st (Op exp1 Or exp2)  = bbop (evExp st exp1) (evExp st exp2) (||)

bbop :: Value -> Value -> (Bool -> Bool -> Bool) -> Value
bbop (B i1) (B i2) opr = B $ i1 `opr` i2
bbop _ _ _ = error "Booleans expected!"

ibop :: Value -> Value -> (Int -> Int -> Bool) -> Value
ibop (I i1) (I i2) opr = B $ i1 `opr` i2
ibop _ _ _ = error "Integers expected!"

op :: Value -> Value -> (Int -> Int -> Int) -> Value
op (I int1) (I int2) opr = I $ int1 `opr` int2
op _ _ _ = error "Integers expected!"

-- Задача 3 -----------------------------------------
evStmt :: StateW -> Stmt -> StateW 
evStmt st (Assign str e) = updValue st str (evExp st e)
evStmt st (Incr str) = updValue st str $ I $ val + 1 where (I val) = getValue st str
evStmt st (If e stm1 stm2) = if bol then evStmt st stm1 else evStmt st stm2
    where (B bol) = evExp st e
evStmt st (While e stm) = if (evExp st e == B True) 
                          then evStmt st (While e stm) else st
evStmt st (For si e sn s) = if (evExp (evStmt st si) e == B True) 
                            then evStmt newst (For empt e sn s)
                            else st
                            where newst = evStmt (evStmt (evStmt st si) sn) s
                                  empt = (Assign "" (Var ""))  
evStmt st (Block blocks stmts) = foldl (evStmt) zero stmts
    where zero = [(i, val) | (i, typ) <- blocks, 
                  let val = if typ == Bt then (B False) else (I 0)] ++ st
-- Задача 4 -----------------------------------------
evProgram :: Program -> StateW
evProgram prog = evStmt [] prog

-- Задача 5 -----------------------------------------
iswfOp :: Bop -> [Type] -> Maybe Type 
iswfOp bop [It, It] 
     | bop == Eql || bop == Ge || bop == Gt || bop == Le || bop == Lt 
       || bop == Plus || bop == Minus || bop == Times || bop == Div= Just Bt
iswfOp bop [Bt, Bt]
     | bop == And || bop == Or = Just Bt
iswfOp _ _ = Nothing

-- Задача 6 -----------------------------------------
iswfExp :: Exp -> VarEnv -> Maybe Type
iswfExp (Var str) ve = Just (getType ve str)
iswfExp (Const val) _ = Just (val2type val)
iswfExp (Op e1 oper e2) ve = case iswfOp oper ((types ve e1) ++ (types ve e2)) of
    Just t -> Just t
    _ -> Nothing

types :: VarEnv -> Exp -> [Type]
types ve (Var str) = (getType ve str):[]
types _ (Const val) = (val2type val):[]
types ve (Op e _ ee) = (types ve e) ++ (types ve ee)

getType:: VarEnv -> String -> Type
getType ve str = snd $ head $ filter (\x -> fst x == str) ve

val2type :: Value -> Type
val2type (I _) = It
val2type (B _) = Bt

-- Задача 7 -----------------------------------------
iswfStmt :: Stmt -> VarEnv -> Bool 
iswfStmt _ [] = True
iswfStmt (Incr str) ve = (getType ve str) == It
iswfStmt (Assign _ _) _ = True
iswfStmt (If e s1 s2) ve = (iswfStmt s1 ve) && (iswfStmt s2 ve) 
    && ((iswfExp e ve) == Just Bt)
iswfStmt (While e s) ve = (iswfStmt s ve) && ((iswfExp e ve) == Just Bt)
iswfStmt (For si e sn s) ve = (iswfStmt si ve) && (iswfStmt sn ve) 
    && (iswfStmt s ve) && ((iswfExp e ve) == Just Bt)
iswfStmt (Block _ stmts) ve = and $ [iswfStmt s ve | s <- stmts]

--------------------------------
iswfProgram :: Program -> Bool 
iswfProgram st@(Block _ _) = iswfStmt st []
iswfProgram _              = False  
---------
{-
   { int b; int e; int out;  
     b := 6; e := 5; out:= 1;
	 { int i; 
       for (i:=0; i<e; i++) out := out*b   
	 }  
   }
-}
power :: Program
power = Block [("b",It),("e",It),("out",It)]
              [Assign "b" (Const (I 6)), Assign "e" (Const (I 5)), Assign "out" (Const(I 1)),
               Block [("i",It)] 
                     [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                           (Assign "out" (Op (Var "out") Times (Var "b")))
                     ]
              ] 

{-
   {int a; int b; 
    a := 317; b := 0;
	{bool c; c:=true; 
	         while(c) {b++; c:= a >= b*b}
	};
    b := b-1
   } 	
-}
squareRoot :: Program
squareRoot = Block [("a",It),("b",It)]
                   [ Assign "a" (Const (I 317)), Assign "b" (Const (I 0)),
                     Block [("c", Bt)] 
                            [Assign "c" (Const (B True)),
                             While (Var "c")
                                 (Block []
                                   [(Incr "b"), 
                                     Assign "c" (Op (Var "a") Ge (Op (Var "b") Times (Var "b")))
                                   ])
                           ],
                     Assign "b" (Op (Var "b") Minus (Const (I 1)))
                   ]

{-
  {int in; int out; in := 121; 
     {int f0; int f1; int c; f0 := 1; f1 := 1;
      if(in == 0) then out := f0 else 
      if (in == 1) then out := f1 else 
        for (c := 2; c <= in; c++) {
         out := f0 + f1; f0 := f1; f1 := out
        }
	 } 	
  }
-}
fibonacci :: Program
fibonacci = Block [("in",It), ("out",It)]
                  [Assign "in" (Const (I 12)),
                   Block [("f0",It), ("f1",It), ("c",It)]
                          [Assign "f0" (Const (I 1)), Assign "f1" (Const (I 1)),
                           If (Op (Var "in") Eql (Const (I 0)))
                              (Assign "out" (Var "f0"))
                              (If (Op (Var "in") Eql (Const (I 1)))
                                  (Assign "out" (Var "f1"))
                                  (For (Assign "c" (Const (I 1)))
                                       (Op (Var "c") Lt (Var "in")) (Incr "c")
                                       (Block []
                                             [ Assign "out" (Op (Var "f0") Plus (Var "f1"))
                                             , Assign "f0" (Var "f1")
                                              , Assign "f1" (Var "out")
                                             ])
                                  )
                              )
                          ]
                  ]