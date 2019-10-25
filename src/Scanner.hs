{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

module Scanner (Token(..), tokenize) where

import Data.Char

data Token = Var String
           | Connective String
           | Param String deriving (Show)

-- Takes a string and breaks it up into a list of tokens
tokenize :: String  -> [Token]
tokenize ('(':r) = (Param "(") : (tokenize r)
tokenize (')':r) = (Param ")") : (tokenize r)
tokenize ('!':r) = (Connective "!") : (tokenize r)
tokenize ('&':r) = (Connective "&") : (tokenize r)
tokenize ('|':r) = (Connective "|") : (tokenize r)
tokenize ('=':'>':r) = (Connective "=>") : (tokenize r)
tokenize ('<':'=':'>':r) = (Connective "<=>") : (tokenize r)
tokenize (h:t)
 | isAlphaNum h = scanProp (Var [h]) t
 | isSpace h = tokenize t
 | otherwise = errorWithoutStackTrace ("\nScan Error. Unsupported charactor: " ++ [h])
tokenize "" = []

-- Helper function that scans and returns an atom 
scanProp :: Token -> String -> [Token]
scanProp (Var n) (h:t)
 | isAlphaNum h = scanProp (Var (n ++ [h])) t
 | otherwise = (Var n) : (tokenize (h : t))
scanProp (Var n) [] = (Var n) : []

