{-# OPTIONS_GHC -Wall #-}
module Lopukhina13 where

import Text.ParserCombinators.Parsec

-- Задача 1 -----------------------------------------
fullBrace :: Parser()
fullBrace = spaces >> brace >> eof

brace :: Parser()
brace = parentheses <|> brackets <|> bracess <|> spaces

parentheses :: Parser()
parentheses = symbol '(' >> spaces >> brace >> symbol ')' >> spaces >> brace

brackets :: Parser()
brackets = symbol '[' >> spaces >> brace >> symbol ']' >> spaces >> brace

bracess :: Parser()
bracess = symbol '{' >> spaces >> brace >> symbol '}' >> spaces >> brace

balance  :: String -> Bool
balance str = either (const False) (const True) (parse fullBrace "" str)
   
-- Задача 2 ----------------------------------------- 
data Bexp = Bvalue Bool | Bvar Char | Not Bexp 
          | And Bexp Bexp | Or Bexp Bexp  deriving (Eq, Show)

fullBe :: Parser Bexp 
fullBe = bexp <* eof

bdis :: Parser Bexp
bdis = symbol '(' *> bexp <* symbol ')'
    <|> do {_ <- string "true"; return $ Bvalue True}
    <|> do {_ <- string "false"; return $ Bvalue False}
    <|> do {symbol '!'; Not <$> bdis}
    <|> Bvar <$> letter

bcon :: Parser Bexp
bcon = chainl1 bdis (do {_ <- symbol '&'; return And})

bexp :: Parser Bexp
bexp = chainl1 bcon (do {_ <- symbol '|'; return Or})

anBexp :: String -> Maybe Bexp
anBexp str = case (parse fullBe "" str) of
                Left _   ->  Nothing
                Right ex -> Just ex

-- Задача 3 ----------------------------------------- 
type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

text :: Parser XML
text = Text <$> (many1 $ noneOf "<>")

value :: Parser String
value = many $ noneOf "\""

fullValue :: Parser String
fullValue = string "\"" *> value <* string "\""

name :: Parser Name
name = do {l <- letter; nms <- many $ digit <|> letter <|> oneOf ".-"; 
           return (l:nms)}

attrib :: Parser Attributes
attrib = many $ do {spaces; n <- name; spaces; _ <- string "="; 
                    spaces; v <- fullValue; return (n,v)}

element :: Parser XML
element = do {_ <- try $ string "<"; n <- name; a <- try $ attrib;
              _ <- try $ string ">";
              x <- try $ many xml; _ <-  try $ string "</"; 
              _ <- try $ name; _ <- try $ string ">";
              return $ Element n a x}

xml :: Parser XML
xml = try (element <|> text)

fullXML :: Parser XML 
fullXML = spaces *> element <* spaces <* eof

anXML :: String -> Maybe XML
anXML str = case (parse fullXML "" str) of
               Left _    -> Nothing
               Right res -> Just res

----------------  Мова SPL  ------------   
data Value = I Int  | B Bool deriving (Show, Eq)
data Exp = Var String      -- Змінна
         | Const Value     -- Константа
         | Op Exp Bop Exp  -- Операція
                 deriving (Show, Eq)

-- Бінарні (2-аргумента) оператори
data Bop =  Plus | Minus | Times | Div   
          | Gt | Ge | Lt | Le| Eql | Ba | Bo -- ba = and, bo = or
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

{- 
  symbol = ';' | '{' | '}' | '(' | ')' 
  identif=  char {digit | char}
  keyword= "int" | "bool" | "if" | "while" | "for" | "else" | "true" | "false"
  iden   =  identif .... not keyword
  number = digit { digit }.
  mulOp  = "*" | "/".
  addOp  = "+" | "-".
  relOp  = "<" | "<=" | ">" | ">=" | "==" 
  disOp  = "&" 
  conOp  = "|"
  typev  = "int" | "bool" 
-}

iden :: Parser String
iden = try(do {nm <- identif;
                if (any(nm==) ["int","bool","if","while","for","else","True","False"])
                    then unexpected ("reserved word " ++ show nm)
                    else return nm 
               }) 

oper  :: String -> Bop -> Parser Bop
oper str bop = do {_ <- string str; return bop}

mulOp :: Parser Bop   
mulOp = oper "*" Times <|> oper "/" Div

disOp :: Parser Bop   
disOp = oper "&" Ba

conOp :: Parser Bop   
conOp = oper "|" Bo

-- обробляє наступні за р "порожні" символи
lexem :: Parser a -> Parser a
lexem p = do {a <- p; spaces; return a}

-- :type Op      --> Exp -> Bop -> Exp -> Exp 
-- :type flip Op --> Bop -> Exp -> Exp -> Exp         
expOp :: Parser Bop -> Parser (Exp -> Exp -> Exp)
expOp p = do {x <- lexem p; return (flip Op x)}

symbol :: Char -> Parser ()
symbol ch = lexem (char ch >> return ())

keyword :: String -> Parser ()
keyword st = try (lexem (string st >> notFollowedBy alphaNum)) 

typev :: Parser Type 
typev = do {keyword "int"; return It}
    <|> do {keyword "bool"; return Bt} 

-- Задача 4 -----------------------------------
identif :: Parser String
identif = do {l <- letter; ltrs <- many (digit <|> letter); return (l:ltrs)}

number :: Parser Int
number = read <$> (many1 digit)
 
addOp :: Parser Bop  
addOp = oper "+" Plus <|> oper "-" Minus

relOp :: Parser Bop  
relOp = try (oper "<=" Le) <|> try (oper ">=" Ge) <|> try (oper "==" Eql)
     <|> try (oper "<" Lt) <|> try (oper ">" Gt)

-- Задача 5 -----------------------------------
{- 
  factor = '(' expr ')' | number | "true" | "false" | iden
  term   = factor { mulOp factor }
  relat  = term   { addOp term }
  conj   = relat  { relOp relat } 
  disj   = conj   { conOp conj }   
  expr   = disj   { disOp disj }
-}

factor :: Parser Exp
factor = do {symbol '('; x <- expr; symbol ')'; return x}
     <|> do {nm <- lexem number; return (Const (I nm))}
     <|> do {keyword "true"; return (Const (B True))}
     <|> do {keyword "false"; return (Const (B False))}
     <|> do {cs <- lexem iden; return (Var cs) }
     <?> "factor"

term :: Parser Exp     
term = chainl1 factor $ expOp mulOp

relat :: Parser Exp
relat = chainl1 term $ expOp addOp

conj :: Parser Exp
conj = chainl1 relat $ expOp relOp

disj :: Parser Exp
disj = chainl1 conj $ expOp conOp

expr :: Parser Exp
expr = chainl1 disj $ expOp disOp

-- Задача 6 -----------------------------------------
{-
  stmt   = "for" forSt | "while" whileSt | "if" ifSt | iden assSt | blockSt  
  forSt  = '(' stmt ';' expr ';' stmt ')' stmt 
  whileSt= '(' expr ')' stmt 
  ifSt   = '(' expr ')' stmt "else" stmt 
  assSt  = "++" | ":=" expr 
  blockSt= '{' {defin} listSt '}' 
  defin  = type iden ';'
  listSt = stmt {';' stmt}  
  program= stmt eos 
-}
   
stmt :: Parser Stmt 
stmt = do {keyword "for"; forSt}
   <|> do {keyword "while"; whileSt}
   <|> do {keyword "if"; ifSt}
   <|> do {var <- lexem iden; assignSt var}
   <|> blockSt
   <?> "statement"

forSt :: Parser Stmt  
forSt = do {symbol '('; si <- lexem stmt; symbol ';';
                         e <- lexem expr; symbol ';'; 
                        sn <- lexem stmt; symbol ')';
                         s <- lexem stmt; return $ For si e sn s}

whileSt :: Parser Stmt               
whileSt = do {symbol '('; e <- lexem expr; symbol ')';
              s <- lexem stmt; return $ While e s} 
              
ifSt :: Parser Stmt              
ifSt = do {symbol '('; e <- lexem expr; symbol ')'; s1 <- lexem stmt;
                        _ <- lexem $ string "else"; s2 <- lexem stmt;
                        return $ If e s1 s2} 

assignSt :: String -> Parser Stmt 
assignSt v = try(do {_ <- lexem $ string "++"; return $ Incr v} <|>
                 do {_ <- lexem $ string ":="; e <- lexem expr; return $ Assign v e})

listSt :: Parser [Stmt]
listSt = do {s <- lexem stmt; stmts <- many (do {symbol ';'; lexem stmt;});
             return (s:stmts)}

defin :: Parser (String, Type)
defin = do {t <- lexem typev; i <- lexem iden; symbol ';'; return (i, t)}

blockSt :: Parser Stmt
blockSt = do {symbol '{'; d <- many $ defin; l <- listSt; symbol '}'; return $ Block d l}
               
---------------------------------------------	
-- ������� �������
---------------------------------------------				
program :: Parser Stmt 
program = do {spaces; r <- stmt; eof; return r}

parseSPL :: String -> Either ParseError Program
parseSPL s = parse program "" s
---------------------------------------------
--- ���� ��� ����������
--------------------------------------------- 
casablanca :: String 
casablanca
  = "<film title=\"Casablanca\">\n  <director>Michael Curtiz</director>\n  <year>1942\
    \</year>\n</film>\n\n\n"

casablancaParsed :: XML 
casablancaParsed
  = Element "film" 
            [("title","Casablanca")] 
            [Text "\n  ",
             Element "director" [] [Text "Michael Curtiz"],
             Text "\n  ",
             Element "year" [] [Text "1942"],
             Text "\n"]

power :: String
power =
   "{ int b; int e; int out; b := 6; e := 5; out:= 1;\
   \  for (i:=0; i<e; i++) out := out*b   \
   \}"

powerAST :: Program 
powerAST = Block [("b",It),("e",It),("out",It)]
              [Assign "b" (Const (I 6)), Assign "e" (Const (I 5)), Assign "out" (Const(I 1)),
               Block [("i",It)] 
                     [For (Assign "i" (Const(I 0))) (Op (Var "i") Lt (Var "e")) (Incr "i")
                           (Assign "out" (Op (Var "out") Times (Var "b")))
                     ]
              ]

squareRoot :: String
squareRoot =
   "{int a; int b; a := 317; b := 0;\
   \  {bool c; c:=true; while(c) {b++; c:= a >= b*b}};\
   \  b := b-1\
   \ }"

squareRootAST :: Program
squareRootAST = Block [("a",It),("b",It)]
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
