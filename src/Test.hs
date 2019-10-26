{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Authors: Soubhk Ghosh (netid: sghosh13), Richard Magnotti (netid: rmagnott)
-}

import System.IO
import Test.HUnit
import Text.Printf
import Reasoning

inference :: Bool -> Bool -> String
inference t f
   | t = "TRUE"
   | f = "FALSE"
   | otherwise = "MAYBE"

runProblem :: String -> String -> String -> IO ()
runProblem testName kb q = do
   printf "\n%s\n" testName
   printf "Knowledge base: %s\n" kb
   printf "Query: %s\n" q

   let resultModelChecking = inference (ttEntails kb q) (ttEntails kb ("!" ++ q))
   
   isResolved1 <- plResolution kb q
   isResolved2 <- plResolution kb ("!" ++ q)
   let resultResolution = inference isResolved1 isResolved2
 
   isSAT1 <- walkSatEntails kb q
   isSAT2 <- walkSatEntails kb ("!" ++ q)
   let resultSAT = inference isSAT1 isSAT2

   -- Model Checking and Resolution should give same result
   runTestTT (test [testName ~: "Query " ++ q ~: resultModelChecking ~=? resultResolution])

   putStrLn $ "Ans with model checking: " ++ resultModelChecking
   putStrLn $ "Ans with resolution: " ++ resultResolution
   putStrLn $ "Ans with SAT checking (WalkSAT): " ++ resultSAT

   putStrLn ""

driver = do
   printf "\n************* Welcome to Automated Reasoning *************\n\nPropositional theorem proving tests\n\nPress Enter to continue...\n"

   hFlush stdout
   getLine

   runProblem "Modus Ponens test."
              "(P => Q) & P"
              "Q"
   -- CNF: (!P | Q) & P  

   printf "\nPress Enter to continue...\n"
   hFlush stdout
   getLine   

   runProblem "Wumpus World test."
              "!P11 & (B11 <=> (P12 | P21)) & (B21 <=> (P11 | P22 | P31)) & !B11 & B21"
              "P12"
   -- CNF: !P11 & (P12 | P21 | !B11) & (!P12 | B11) & (!P21 | B11) & (P11 | P22 | P31 | !B21) & (!P11 | B21) & (!P22 | B21) & (!P31 | B21) & !B11 & B21

   printf "\nPress Enter to continue...\n"
   hFlush stdout
   getLine

   let (testName, kb) = ("Horn Clauses test.", "(Y => I) & (!Y => (!I & M)) & ((I | M) => H) & (H => G)")
   -- Mythical: Y
   -- Immortal: I
   -- Mammal: M
   -- Horned: H
   -- Magical: G
 
   -- CNF: (!Y | I) & (!I | Y) & (M | Y) & (!I | H) & (!M | H) & (!H | G)

   runProblem testName kb "Y"

   runProblem testName kb "G"

   runProblem testName kb "H"

   printf "\nPress Enter to continue...\n"
   hFlush stdout
   getLine

   let (testName, kb) = ("The Labyrinth Test.", "!(G & (S => M)) & !(!G & !S) & !(G & !M)")
   -- Gold: G
   -- Marble: M
   -- Stones: S

   -- CNF: (S | !G) & (!M | !G) & (G | S) & (!G | M)
   runProblem testName kb "G"

   runProblem testName kb "M"

   runProblem testName kb "S"

   printf "\nPress Enter to continue...\n"
   hFlush stdout
   getLine

   -- May not be correct (WIP)
   let (testName, kb) = ("The Doors of Enlightenment. Smullyan’s problem", 
        "(A <=> X) & (B <=> (Y | Z)) & (C <=> (A & B)) & (D <=> (X & Y)) & (E <=> (X & Z)) & (F <=> (D | E)) & (G <=> (C => F)) & (H <=> ((G & H) => A))")

   runProblem testName kb "X"

   runProblem testName kb "Y"

   runProblem testName kb "Z"
 
   printf "\nPress Enter to continue...\n"
   hFlush stdout
   getLine

   -- May not be correct (WIP)
   let (testName, kb) = ("The Doors of Enlightenment. Liu’s problem",
        "(A <=> X) & (H <=> ((G & H) => A)) & (C <=> (A & (B | C | D | E | F | G | H))) & (G <=> (C => (Anything)))")

   runProblem testName kb "X"

   runProblem testName kb "Y"

   runProblem testName kb "Z"

   -- All other tests goes here
   -- 
   -- Test #1
   -- runProblem <testName:String> <Knowledge Base:String> <Query:String>
   --
   -- Test #2
   -- runProblem <testName:String> <Knowledge Base:String> <Query:String>
   --
   -- ...

main = driver

