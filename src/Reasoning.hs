{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

module Reasoning (ttEntails, parse, plResolution, walkSat, getCNFClauses, genCNF) where

import Data.Map as Map
import Data.List as List
import Data.Set as Set

import Data.Function (on)
import System.Random

import Scanner
import Formula
import Parser
import CNF_converter

type Model = Map String Bool

symbols :: Prop -> [String]
symbols (propl :<=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :|: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :&: propr) = (symbols propl) ++ (symbols propr)
symbols (Not prop) = symbols prop
symbols (Atom p) = [p]

plTrue :: Prop -> Model -> Bool
plTrue (propl :<=>: propr) model = (not alpha || beta) && (not beta || alpha)
    where alpha = (plTrue propl model)
          beta = (plTrue propr model)
plTrue (propl :=>: propr) model = not (plTrue propl model) || (plTrue propr model)
plTrue (propl :|: propr) model = (plTrue propl model) || (plTrue propr model)
plTrue (propl :&: propr) model = (plTrue propl model) && (plTrue propr model)
plTrue (Not prop) model = not (plTrue prop model)
plTrue (Atom p) model = let (Just a) = Map.lookup p model in a

ttCheckAll :: Prop -> Prop -> [String] -> Model -> Bool
ttCheckAll kb alpha symbols model
    | symbols == [] = (if (plTrue kb model) then (plTrue alpha model) else True)
    | otherwise = let (p:rest) = symbols in ((ttCheckAll kb alpha rest $ Map.insert p True model) && (ttCheckAll kb alpha rest $ Map.insert p False model))

ttEntails :: Prop -> Prop -> Bool
ttEntails kb alpha = ttCheckAll kb alpha uniqueSymbols Map.empty
    where uniqueSymbols = List.nub (symbols kb ++ symbols alpha)


literalsInClause :: Prop -> Set Prop
literalsInClause (Atom p) = Set.singleton (Atom p)
literalsInClause (Not prop) = Set.singleton (Not prop)
literalsInClause (propl :|: propr) = (literalsInClause propl) `Set.union` (literalsInClause propr)
literalsInClause _ = Set.empty


getCNFClauses :: Prop -> [Prop]
getCNFClauses (propl :&: propr) = (getCNFClauses propl) ++ (getCNFClauses propr)
getCNFClauses prop = [prop]



choice1 :: [String] -> Model -> IO Model
choice1 csymbols model = do
    i <- randomRIO (0, length csymbols - 1)
    let sym = csymbols !! i
    let (Just val) = Map.lookup sym model
    let flippedModel = Map.insert sym (not(val)) model
    return flippedModel

choice2 :: [Prop] -> [String] -> Model -> IO Model
choice2 clauses csymbols model = do
    let lclauses = List.map (Set.toList . literalsInClause) clauses
    let satisfyCountMap = [(sym, length (List.filter (\c -> if val then (Not (Atom sym)) `elem` c else 
                                                     (Atom sym) `elem` c) lclauses)) | sym <- csymbols, let (Just val) = Map.lookup sym model]
    let sym = fst (List.maximumBy (compare `on` snd) satisfyCountMap)
    let (Just val) = Map.lookup sym model
    let flippedModel = Map.insert sym (not(val)) model
    return flippedModel


applyFlip :: Int -> Model -> [Prop] -> Float -> IO Bool
applyFlip maxFlips model clauses p = do
     if maxFlips == 0 then return False
     else do
         let falseClauses = List.filter (\c -> not (plTrue c model)) clauses
         if falseClauses == [] then return True
         else do
            i <- randomRIO (0, length falseClauses - 1)

            let chooser = (replicate (round $ p * 100) choice1) ++ (replicate (round $ (100 - p * 100)) (choice2 clauses))

            j <- randomRIO (0, 99)

            flippedModel <- (chooser !! j) (symbols $ falseClauses !! i) model

            result <- applyFlip (maxFlips - 1) flippedModel clauses p
            return result

-- For checking satisfiability
walkSat :: [Prop] -> Float -> Int -> IO Bool
walkSat clauses p maxFlips = do
    g <- newStdGen
    let model = Map.fromList (List.zip (List.foldl (\acc c -> acc `List.union` (symbols c)) [] clauses) (randoms g :: [Bool]))
    result <- applyFlip maxFlips model clauses p
    return result


-- Assumes prop to be in CNF
plResolve :: Set Prop -> Set Prop -> [Set Prop]
plResolve ci cj = List.filter (not . checkTautology) [Set.union (Set.delete x ci) (Set.delete nx cj) | x <- Set.toList ci, let nx = (applyDeMorgan (Not x)), Set.member nx cj]
    where checkTautology p = True `elem` [(applyDeMorgan (Not x)) == y | (x:ys) <- tails (Set.toList p), y <- ys]


plResolution :: Prop -> Prop -> Bool
plResolution kb alpha = loopTillDone (Set.map (literalsInClause) (Set.fromList . getCNFClauses . genCNF $ (kb) :&: (Not alpha))) Set.empty
    where loopTillDone clauses new
              | Set.member Set.empty resolvents = True
              | otherwise =
                let nnew = new `Set.union` resolvents in
                if (nnew `Set.isSubsetOf` clauses) then False
                else loopTillDone (clauses `Set.union` nnew) nnew
                where resolvents = Set.fromList . concat $ [plResolve x y | (x:ys) <- tails (Set.toList clauses), y <- ys]
