{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Authors: Soubhk Ghosh (netid: sghosh13), Richard Magnotti (netid: rmagnott)
-}

module Reasoning (ttEntails, plResolution, walkSatEntails) where

import Data.Map as Map
import Data.List as List
import Data.Set as Set

import Data.Function (on)
import System.Random
import System.Process

import Scanner
import Formula
import Parser
import CNF_converter

-- Data type for the Model
type Model = Map String Bool

genCNFPython :: Prop -> IO Prop
genCNFPython p = do
   -- TODO --
   cnf <- readProcess "python3" ["cnf.py", (show p)] ""
   return (parse cnf)

-- Returns the list of symbols in a Formula
symbols :: Prop -> [String]
symbols (propl :<=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :=>: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :|: propr) = (symbols propl) ++ (symbols propr)
symbols (propl :&: propr) = (symbols propl) ++ (symbols propr)
symbols (Not prop) = symbols prop
symbols (Atom p) = [p]

-- Returns true if the Formula holds within a model
plTrue :: Prop -> Model -> Bool
plTrue (propl :<=>: propr) model = (plTrue propl model) == (plTrue propr model)
plTrue (propl :=>: propr) model = not (plTrue propl model) || (plTrue propr model)
plTrue (propl :|: propr) model = (plTrue propl model) || (plTrue propr model)
plTrue (propl :&: propr) model = (plTrue propl model) && (plTrue propr model)
plTrue (Not prop) model = not (plTrue prop model)
plTrue (Atom p) model = let (Just a) = Map.lookup p model in a

--Basic Model Checking: Truth-table enumeration algorithm
ttEntails :: String -> String -> Bool
ttEntails kb query = let (kbTree, qTree) = (parse kb, parse query) in ttCheckAll kbTree qTree (List.nub $ symbols kbTree ++ symbols qTree) Map.empty
    where ttCheckAll kb alpha symbols model
              | symbols == [] = (if (plTrue kb model) then (plTrue alpha model) else True)
              | otherwise = let (p:rest) = symbols in ((ttCheckAll kb alpha rest $ Map.insert p True model) && 
                                                      (ttCheckAll kb alpha rest $ Map.insert p False model))

loopTillDone clauses new
    | Set.member Set.empty resolvents = True
    | otherwise =
      let nnew = new `Set.union` resolvents in
      if (nnew `Set.isSubsetOf` clauses) then False
      else loopTillDone (clauses `Set.union` nnew) nnew
      where resolvents = Set.fromList . concat $ [plResolve x y | (x:ys) <- tails (Set.toList clauses), y <- ys]

-- Advanced Propositional Inference: Propositional Resolution
plResolution :: String -> String -> IO Bool
plResolution kb query = do
    let (kbTree, qTree) = (parse kb, parse query)
    -- TODO --
    let cnfForm = genCNF $ (kbTree) :&: (Not qTree) 
    let result = loopTillDone (Set.map (Set.fromList . literalsInClause) (Set.fromList . getCNFClauses $ cnfForm)) Set.empty
    return result


-- Returns the list of all possible clauses obtained by resolving its 2 input clauses
-- Assumes prop to be in CNF
plResolve :: Set Prop -> Set Prop -> [Set Prop]
plResolve ci cj = List.filter (\p -> not . or $ [tautology p, longerClause p]) 
    [Set.union (Set.delete x ci) (Set.delete nx cj) | x <- Set.toList ci, let nx = (applyDeMorgan (Not x)), Set.member nx cj]
    where tautology p = True `elem` [(applyDeMorgan (Not x)) == y | (x:ys) <- tails (Set.toList p), y <- ys]
          longerClause p = (Set.size p >= Set.size ci) && (Set.size p >= Set.size cj)


walkSatEntails :: String -> String -> IO Bool
walkSatEntails kb query = do
   -- TODO --
   let cnfForm = genCNF . parse $ "(" ++ kb ++ ")" ++ "& !" ++ "(" ++ query ++ ")"
   res <- walkSat (getCNFClauses cnfForm) 0.5 10000
   return (not res)

-- Advanced Propositional Inference: WalkSAT algorithm for checking satisfiability
-- Assumes list of clauses to be in CNF
walkSat :: [Prop] -> Float -> Int -> IO Bool
walkSat clauses p maxFlips = do
    v <- newStdGen
    let model = Map.fromList (List.zip (List.foldl (\acc c -> acc `List.union` (symbols c)) [] clauses) (randoms v :: [Bool]))
    result <- applyFlip maxFlips model clauses p
    return result

-- Helper recursive function that runs 'max_flips' times
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

-- First random choice: Picks a symbol randomly to flip
choice1 :: [String] -> Model -> IO Model
choice1 csymbols model = do
    i <- randomRIO (0, length csymbols - 1)
    let sym = csymbols !! i
    let (Just val) = Map.lookup sym model
    let flippedModel = Map.insert sym (not val) model
    return flippedModel

-- Second random choice: Picks a symbol that maximizes the number of satisfied clauses after a flip
choice2 :: [Prop] -> [String] -> Model -> IO Model
choice2 clauses csymbols model = do
    let lclauses = List.map literalsInClause clauses
    let satisfyCountMap = [(sym, length (List.filter (\c -> if val then (Not (Atom sym)) `elem` c else
                                                     (Atom sym) `elem` c) lclauses)) | sym <- csymbols, let (Just val) = Map.lookup sym model]
    let sym = fst (List.maximumBy (compare `on` snd) satisfyCountMap)
    let (Just val) = Map.lookup sym model
    let flippedModel = Map.insert sym (not(val)) model
    return flippedModel
