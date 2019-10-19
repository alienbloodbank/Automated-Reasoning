{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

module Scanner (Token(..), scan, tokenize) where

import System.IO
import Data.Char

data Token = Var String
           | Connective String
           | Param String deriving (Show)

--Takes a file specified by the file path and returns a list of tokens
scan :: FilePath -> IO [Token]
scan path = do
    contents <- readFile path
    return (tokenize contents)

--Takes a string and breaks it up into a list of tokens
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
 | otherwise = tokenize t
tokenize "" = []

scanProp :: Token -> String -> [Token]
scanProp (Var n) (h : t)
 | isAlphaNum h = scanProp (Var (n ++ [h])) t
 | otherwise = (Var n) : (tokenize (h : t))
scanProp (Var n) [] = (Var n) : []


