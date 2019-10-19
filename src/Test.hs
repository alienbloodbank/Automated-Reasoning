{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

import Test.HUnit
import System.CPUTime
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

   isSAT1 <- walkSat (getCNFClauses . genCNF . parse $ "(" ++ kb ++ ")" ++ "&& !" ++ "(" ++ q ++ ")") 0.5 1000
    
   isSAT2 <- walkSat (getCNFClauses . genCNF . parse $ "(" ++ kb ++ ")" ++ "&& " ++ "(" ++ q ++ ")") 0.5 1000
    
   let resultModelChecking = inference (ttEntails (parse kb) (parse q)) (ttEntails (parse kb) (parse $ "!" ++ q))
   
   start <- getCPUTime
   let resultResolution = inference (plResolution (parse kb) (parse q)) (plResolution (parse kb) (parse $ "!" ++ q))
   end <- getCPUTime

   let diff = (fromIntegral (end - start)) / (10^12)
   printf "Computation time: %0.3f sec\n" (diff :: Double)
   
   let resultSAT = inference (not isSAT1) (not isSAT2)

   runTestTT (test [testName ~: "Query " ++ q ~: resultModelChecking ~=? resultResolution])

   putStrLn $ "Ans with model checking: " ++ resultModelChecking
   putStrLn $ "Ans with resolution: " ++ resultResolution
   putStrLn $ "Ans with SAT checking: " ++ resultSAT

   putStrLn ""

driver = do
   runProblem "Modus Ponens test."
              "(P => Q) && P"
              "Q"

   runProblem "Wumpus World test."
              "!P11 && (B11 <=> (P12 || P21)) && (B21 <=> (P11 || P22 || P31)) && !B11 && B21"
              "P12"

   let (testName, kb) = ("Horn Clauses test.", "(Y => I) && (!Y => (!I && M)) && ((I || M) => H) && (H => G)")
   -- Mythical: Y
   -- Immortal: I
   -- Mammal: M
   -- Horned: H
   -- Magical: G
   runProblem testName kb "Y"

   runProblem testName kb "G"

   runProblem testName kb "H"

   let (testName, kb) = ("The Labyrinth Test.", "!(G && (S => M)) && !(!G && !S) && !(G && !M)")
   -- Gold: G
   -- Marble: M
   -- Stones: S
   runProblem testName kb "G"

   runProblem testName kb "M"

   runProblem testName kb "S"

   putStrLn "The Doors of Enlightenment."

   -- May not be correct (WIP)
   let (testName, kb) = ("Smullyan’s problem", 
        "(A <=> X) && (B <=> (Y || Z)) && (C <=> (A && B)) && (D <=> (X && Y)) && (E <=> (X && Z)) && (F <=> (D || E)) && (G <=> (C => F)) && (H <=> ((G && H) => A))")

   runProblem testName kb "X"

   runProblem testName kb "Y"

   runProblem testName kb "Z"

   -- May not be correct (WIP)
   let (testName, kb) = ("Liu’s problem",
        "(A <=> X) && (H <=> ((G && H) => A)) && (C <=> (A && (B || C || D || E || F || G || H))) && G")

   runProblem testName kb "X"

   runProblem testName kb "Y"

   runProblem testName kb "Z"

main = driver
