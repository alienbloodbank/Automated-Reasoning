{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

module Reasoning (ttEntails, parse, plResolution) where

import Data.Map as Map
import Data.List as List
import Data.Set as Set

import Scanner
import Formula
import Parser
import CNF_converter

type Model = Map String Bool

symbols :: Prop -> [String]
symbols (propl :<=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :||: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :&&: propr) = (symbols propl) ++ (symbols propr)
symbols (Not prop) = symbols prop
symbols (Symbol p) = p:[]

plTrue :: Prop -> Model -> Bool
plTrue (propl :<=>: propr) model = (not alpha || beta) && (not beta || alpha)
    where alpha = (plTrue propl model)
          beta = (plTrue propr model)
plTrue (propl :=>: propr) model = not (plTrue propl model) || (plTrue propr model)
plTrue (propl :||: propr) model = (plTrue propl model) || (plTrue propr model)
plTrue (propl :&&: propr) model = (plTrue propl model) && (plTrue propr model)
plTrue (Not prop) model = not (plTrue prop model)
plTrue (Symbol p) model = let (Just a) = Map.lookup p model in a

ttCheckAll :: Prop -> Prop -> [String] -> Model -> Bool
ttCheckAll kb alpha symbols model
    | List.null symbols = (if (plTrue kb model) then (plTrue alpha model) else True)
    | otherwise = let (p:rest) = symbols in ((ttCheckAll kb alpha rest $ Map.insert p True model) && (ttCheckAll kb alpha rest $ Map.insert p False model))

ttEntails :: Prop -> Prop -> Bool
ttEntails kb alpha = ttCheckAll kb alpha uniqueSymbols Map.empty
    where uniqueSymbols = List.nub (symbols kb ++ symbols alpha)


-- Assumes clause to only have disjunctions
literalsInClause :: Prop -> [Prop]
literalsInClause (Symbol p) = [Symbol p]
literalsInClause (Not prop) = [Not prop]
literalsInClause (propl :||: propr) = (literalsInClause propl) ++ (literalsInClause propr)
literalsInClause _ = []

-- Assumes prop to be in CNF
getCNFClauses :: Prop -> [Prop]
getCNFClauses (propl :&&: propr) = (getCNFClauses propl) ++ (getCNFClauses propr)
getCNFClauses prop = [prop]


--checkTautology :: Set Prop -> Bool
--checkTautology p = [ | (x:ys) <- tails (Set.toList p), y <- ys]


-- Assumes prop to be in CNF
plResolve :: Set Prop -> Set Prop -> [Set Prop]
plResolve ci cj = [Set.union (Set.delete x ci) (Set.delete nx cj)| x <- Set.toList ci, let nx = (applyDeMorgan (Not x)), Set.member nx cj]


-- Assumes prop to be in CNF
plResolution :: Prop -> Prop -> Bool
plResolution kb alpha = loopTillDone clauses []
    where clauses = List.map (Set.fromList . literalsInClause) (getCNFClauses . genCNF $ (kb) :&&: (Not alpha))
          loopTillDone clauses new
              | Set.empty `elem` resolvents = True
              | otherwise = 
                let nnew = new `List.union` resolvents in
                (if (Set.fromList nnew `Set.isSubsetOf` Set.fromList clauses) then False else (loopTillDone (clauses `List.union` nnew) nnew))
                where resolvents = concat [(plResolve x y) | (x:ys) <- tails clauses, y <- ys]


