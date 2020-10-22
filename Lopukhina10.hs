{-# OPTIONS_GHC -Wall #-}
module Lopukhina10 where

import Data.Char(isSpace, isDigit, isLetter) 

type Name       = String
type Attributes = [(String, String)]
data XML        =  Text String | Element Name Attributes [XML] deriving (Eq, Show)

-- Задача 1 -----------------------------------------
spaces :: String -> String
spaces = dropWhile isSpace
  
-- Задача 2 ----------------------------------------- 
manyT, value, manyN :: String -> (String,String)
-- <sT> ::= довільний символ, крім символів ‘<’ і ‘>’
-- <sV> ::= довільний символ крім символу  ‘”’
-- <sN> :: isDigit isLetter . -
sT, sV, sN :: Char -> Bool
sT x = x /= '<' && x /= '>'
sV x = x /= '"'
sN x = isDigit x || isLetter x || x == '.' || x == '-'

manyT s = (takeWhile sT s, dropWhile sT s)
value s = (takeWhile sV s, dropWhile sV s)
manyN s = (takeWhile sN s, dropWhile sN s)

-- Задача 3 -----------------------------------------
name, text, fullValue :: String ->  Maybe(String,String) 
name "" = Nothing
-- <name> ::= <sL> <manyN> 
name s@(x:_)
    | isLetter x = Just(lx, st)
    | otherwise = Nothing
    where (lx, st) = manyN s

-- <text> ::= <sT> <manyT>
text s
    | not $ null lx = Just(lx, st)
    | otherwise = Nothing
    where (lx,st) = manyT s

-- <fullVal> ::= ‘”’ <value> ‘”’ 
fullValue "" = Nothing
fullValue (x:xs)
    | present = Just(lx, st)
    | otherwise = Nothing
    where lx = fst $ value xs
          st = tail $ snd $ value xs
          present = x == '"' && elem '"' xs

-- Задача 4 -----------------------------------------
-- <attrib> ::= <name> <spaces> ‘=’ <spaces> <fullVal> <spaces>
attrib :: String -> Maybe ((String,String),String) 
attrib s = case name s of
    Just (_, "") -> Nothing
    Just (nm, afterName) -> if l /= '=' then Nothing
                            else case fullValue $ spaces $ lefted of
                            Just (val, str) -> Just((nm, val), spaces $ str)
                            _  -> Nothing
                            where (l:lefted) = spaces afterName
    _ -> Nothing

-- <manyAtt> ::= <attrib> <manyAtt> | ε
manyAtt :: String -> Maybe (Attributes,String)
manyAtt s = Just (listAttributes s)

listAttributes :: String -> (Attributes, String)
listAttributes s = case attrib s of
    Just (a, str) -> (a:attr, lefted)
     where (attr, lefted) = listAttributes str
    Nothing -> ([], s)

-- Задача 5 -----------------------------------------
-- <begTag> ::= ‘<’ <name>  <space>  <manyAtt> ’>’
begTag :: String -> Maybe ((String,Attributes),String)
begTag "" = Nothing
begTag s@(_:tag)
    | isTag s = case name tag of
                Just (nm, afterName) -> case manyAtt $ spaces $ afterName of
                                        Just (attributes, (_:lefted)) -> Just ((nm, attributes), lefted) 
                                        _ -> Nothing
                _ -> Nothing
    | otherwise = Nothing

isTag, isEndTag :: [Char] -> Bool
isTag s = elem '<' s && elem '>' s

-- <endTag> ::= ‘<’’/' <name> ‘>’
endTag :: String -> Maybe (String,String) 
endTag s
    | isEndTag s = case name $ drop 2 s of 
                   Just(nm, afterName) -> Just(nm, tail $ afterName)
                   _ -> Nothing
    | otherwise = Nothing   

isEndTag "" = False
isEndTag [_] = False
isEndTag (a:b:c) = a == '<' && b == '/' && elem '>' c

-- Задача 6 -----------------------------------------
-- XML = Text String | Element Name Attributes [XML]
-- <element> ::= <begTag> <manyXML> <endTag>
element :: String -> Maybe (XML,String) 
element s = case begTag s of
            Just ((nm, attr), other) -> case manyXML other of
                                   Just (someXML, lefted) -> case endTag lefted of
                                                         Just(end, str) -> if nm == end then Just ((Element nm attr someXML), str)
                                                                           else Nothing
                                                         _ -> Nothing
                                   _ -> Nothing
            _ -> Nothing
         
-- <xml> ::= <element> | <text>
xml :: String -> Maybe (XML,String)
xml s = case text s of
        Just (txt, str) -> Just (Text txt, str)
        _  -> case element s of
              Just el -> Just el
              _ -> Nothing

-- <manyXML> ::= <xml> <manyXML> | ε
manyXML :: String -> Maybe ([XML],String)
manyXML s 
    | null xmls = if endTag str == Nothing then Nothing 
                  else Just (xmls, str)           
    | otherwise = case last xmls of
                  Text _  ->  if endTag str == Nothing then Nothing else Just (xmls, str)
                  _  -> Just (xmls, str)
    where (xmls, str) = listXML s

listXML :: String -> ([XML], String)
listXML s = case xml s of
            Just (x, lefted) -> (x:xmls, other)
             where (xmls, other) = listXML lefted
            Nothing -> ([], s)

-- Задача 7 -----------------------------------------
-- <fullXML> ::= <spaces> <element> <spaces>  ‘eos’
fullXML :: String -> Maybe XML 
fullXML s = case element (spaces s) of  
            Just (xm,s1) -> if null (spaces s1) then Just xm else Nothing 
            Nothing -> Nothing  


-- Тестові дані -------------------------------------------
-- Прості тести XML-об'єктів (без проміжків)
stst1, stst2, stst3 :: String
stst1 = "<a>A</a>"
stst2 = "<a x=\"1\"><b>A</b><b>B</b></a>"
stst3 = "<a>\
      \<b>\
        \<c att=\"att1\">text1</c>\
        \<c att=\"att2\">text2</c>\
      \</b>\
      \<b>\
        \<c att=\"att3\">text3</c>\
        \<d>text4</d>\
      \</b>\
    \</a>" 

-- Результати аналізу попередніх XML-об'єктів
x1, x2, x3 :: XML
x1 = Element "a" [] [Text "A"]
x2 = Element "a"
            [("x","1")]
            [Element "b" [] [Text "A"],
             Element "b" [] [Text "B"]]
x3 = Element "a" 
            [] 
            [Element "b" 
                     [] 
                     [Element "c"
                              [("att","att1")] 
                              [Text "text1"],
                      Element "c" 
                              [("att","att2")]
                              [Text "text2"]],
             Element "b" 
                     [] 
                     [Element "c" 
                              [("att","att3")] 
                              [Text "text3"],
                      Element "d" 
                              [] 
                              [Text "text4"]]]

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