{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

module Reasoning (Prop(..), ttEntails) where

import Data.Map as Map
import Data.List as List

{-
infixr :!: 9 
infixl :&&: 8
infixl :||: 7
infixl :=>: 6
infixl :<=>: 5
-}
data Prop = Symbol String 
         | T 
         | F 
         | Not Prop
         | Prop :&&: Prop 
         | Prop :||: Prop 
         | Prop :=>: Prop 
         | Prop :<=>: Prop deriving (Eq)

instance Show Prop where
    show (Symbol p) = p
    show (Not f) = "¬" ++ show f
    show (f :&&: g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
    show (f :||: g)     = "(" ++ show f ++ " ∨ " ++ show g ++ ")"
    show (f :=>: g) = "(" ++ show f ++ " => " ++ show g ++ ")"
    show (f :<=>: g) = "(" ++ show f ++ " <=> " ++ show g ++ ")"

type Model = Map String Bool

symbols :: Prop -> [String]
symbols (Symbol p) = p:[]
symbols (Not prop) = symbols prop
symbols (propl :&&: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :||: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :<=>: propr) = (symbols propl) ++ (symbols propr)
symbols _ = [] 

plTrue :: Prop -> Model -> Bool
plTrue (Symbol p) model = let (Just a) = Map.lookup p model in a
plTrue (Not prop) model = not (plTrue prop model)
plTrue (propl :&&: propr) model = (plTrue propl model) && (plTrue propr model)
plTrue (propl :||: propr) model = (plTrue propl model) || (plTrue propr model)
plTrue (propl :=>: propr) model = not (plTrue propl model) || (plTrue propr model)
plTrue (propl :<=>: propr) model = (not alpha || beta) && (not beta || alpha) 
    where alpha = (plTrue propl model) 
          beta = (plTrue propr model)
plTrue T _ = True
plTrue F _ = False

ttCheckAll :: Prop -> Prop -> [String] -> Model -> Bool
ttCheckAll kb alpha symbols model
    | List.null symbols = (if (plTrue kb model) then (plTrue alpha model) else True) 
    | otherwise = let (p:rest) = symbols in ((ttCheckAll kb alpha rest $ Map.insert p True model) && (ttCheckAll kb alpha rest $ Map.insert p False model)) 

ttEntails :: Prop -> Prop -> Bool
ttEntails kb alpha = ttCheckAll kb alpha uniqueSymbols Map.empty
    where uniqueSymbols = List.nub (symbols kb ++ symbols alpha)

