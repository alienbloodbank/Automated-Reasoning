{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

import Reasoning

inference :: Bool -> Bool -> String
inference t f
   | t = "TRUE"
   | f = "FALSE"
   | otherwise = "MAYBE"

main = do
   putStrLn "\nModus Ponens test."
   putStrLn "Knowledge base: (P => Q) && P"
   putStrLn "Query: Q"

   let kb1 = "(P => Q) && P"
   let r11 = inference (ttEntails (parse kb1) (parse "Q")) (ttEntails (parse kb1) (parse "!Q"))
   let r12 = inference (plResolution (parse kb1) (parse "Q")) (plResolution (parse kb1) (parse "!Q"))
   putStrLn $ "Ans with model checking: " ++ r11
   putStrLn $ "Ans with resolution: " ++ r12   

   putStrLn "\nWumpus World test."
   putStrLn "Knowledge base: !P11 && (B11 <=> (P12 || P21)) && (B21 <=> (P11 || P22 || P31)) && !B11 && B21"
   putStrLn "Query: P12"

   let kb2 = "!P11 && (B11 <=> (P12 || P21)) && (B21 <=> (P11 || P22 || P31)) && !B11 && B21"
   let r21 = inference (ttEntails (parse kb2) (parse "P12")) (ttEntails (parse kb2) (parse "!P12"))
   let r22 = inference (plResolution (parse kb2) (parse "P12")) (plResolution (parse kb2) (parse "!P12"))
   putStrLn $ "Ans with model checking: " ++ r21
   putStrLn $ "Ans with resolution: " ++ r22   

   putStrLn "\nHorn Clauses test."
   -- Mythical: Y
   -- Immortal: I
   -- Mammal: M
   -- Horned: H
   -- Magical: G

   putStrLn "Knowledge base: (Y => I) && (!Y => !I) && ((I || M) => H) && (H => G)"
   putStrLn "Query: Mythical"
   let kb3 = "(Y => I) && (!Y => !I) && ((I || M) => H) && (H => G)"
   let isMythical1 = inference (ttEntails (parse kb3) (parse "Y")) (ttEntails (parse kb3) (parse "!Y"))
   let isMythical2 = inference (plResolution (parse kb3) (parse "Y")) (plResolution (parse kb3) (parse "!Y"))
   putStrLn $ "Ans with model checking: " ++ isMythical1
   putStrLn $ "Ans with resolution: " ++ isMythical2
   
   putStrLn "Query: Magical"
   let isMagical1 = inference (ttEntails (parse kb3) (parse "G")) (ttEntails (parse kb3) (parse "!G"))
   let isMagical2 = inference (plResolution (parse kb3) (parse "G")) (plResolution (parse kb3) (parse "!G"))
   putStrLn $ "Ans with model checking: " ++ isMagical1
   putStrLn $ "Ans with resolution: " ++ isMagical2

   putStrLn "Query: Horned"
   let isHorned1 = inference (ttEntails (parse kb3) (parse "H")) (ttEntails (parse kb3) (parse "!H"))
   let isHorned2 = inference (plResolution (parse kb3) (parse "H")) (plResolution (parse kb3) (parse "!H"))
   putStrLn $ "Ans with model checking: " ++ isHorned1 
   putStrLn $ "Ans with model checking: " ++ isHorned2
     
