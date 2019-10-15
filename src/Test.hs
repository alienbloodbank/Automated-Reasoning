{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

import Test.HUnit
import Reasoning

inference :: Bool -> Bool -> String
inference t f
   | t = "TRUE"
   | f = "FALSE"
   | otherwise = "MAYBE"

main = do
   putStrLn "Modus Ponens test."
   putStrLn "Knowledge base: (P => Q) && P"
   putStrLn "Query: Q"

   let kb1 = "(P => Q) && P"
   let r11 = inference (ttEntails (parse kb1) (parse "Q")) (ttEntails (parse kb1) (parse "!Q"))
   let r12 = inference (plResolution (parse kb1) (parse "Q")) (plResolution (parse kb1) (parse "!Q"))
   
   let test1 = "test1" ~: "Query Q" ~: r12 ~=? r11
   
   putStrLn $ "Ans with model checking: " ++ r11
   putStrLn $ "Ans with resolution: " ++ r12   
    
   putStrLn "\nWumpus World test."
   putStrLn "Knowledge base: !P11 && (B11 <=> (P12 || P21)) && (B21 <=> (P11 || P22 || P31)) && !B11 && B21"
   putStrLn "Query: P12"

   let kb2 = "!P11 && (B11 <=> (P12 || P21)) && (B21 <=> (P11 || P22 || P31)) && !B11 && B21"
   let r21 = inference (ttEntails (parse kb2) (parse "P12")) (ttEntails (parse kb2) (parse "!P12"))
   let r22 = inference (plResolution (parse kb2) (parse "P12")) (plResolution (parse kb2) (parse "!P12"))
   
   let test2 = "test2" ~: "Query P12" ~: r22 ~=? r21

   putStrLn $ "Ans with model checking: " ++ r21
   putStrLn $ "Ans with resolution: " ++ r22   

   putStrLn "\nHorn Clauses test."
   -- Mythical: Y
   -- Immortal: I
   -- Mammal: M
   -- Horned: H
   -- Magical: G

   putStrLn "Knowledge base: (Y => I) && (!Y => !I) && ((I || M) => H) && (H => G)"
   putStrLn "Query: Mythical (Y)"
   let kb3 = "(Y => I) && (!Y => !I) && ((I || M) => H) && (H => G)"
   let isMythical1 = inference (ttEntails (parse kb3) (parse "Y")) (ttEntails (parse kb3) (parse "!Y"))
   let isMythical2 = inference (plResolution (parse kb3) (parse "Y")) (plResolution (parse kb3) (parse "!Y"))
   
   let test3 = "test3" ~: "Query Mythical" ~: isMythical2 ~=? isMythical1
   
   putStrLn $ "Ans with model checking: " ++ isMythical1
   putStrLn $ "Ans with resolution: " ++ isMythical2
   
   putStrLn "Query: Magical (G)"
   let isMagical1 = inference (ttEntails (parse kb3) (parse "G")) (ttEntails (parse kb3) (parse "!G"))
   let isMagical2 = inference (plResolution (parse kb3) (parse "G")) (plResolution (parse kb3) (parse "!G"))

   let test4 = "test4" ~: "Query Magical" ~: isMagical2 ~=? isMagical1
   
   putStrLn $ "Ans with model checking: " ++ isMagical1
   putStrLn $ "Ans with resolution: " ++ isMagical2

   putStrLn "Query: Horned (H)"
   let isHorned1 = inference (ttEntails (parse kb3) (parse "H")) (ttEntails (parse kb3) (parse "!H"))
   let isHorned2 = inference (plResolution (parse kb3) (parse "H")) (plResolution (parse kb3) (parse "!H"))
   
   let test5 = "test5" ~: "Query Horned" ~: isHorned2 ~=? isHorned1
   
   putStrLn $ "Ans with model checking: " ++ isHorned1 
   putStrLn $ "Ans with model checking: " ++ isHorned2

   putStrLn "\nThe Labyrinth Test."
   -- Gold: G
   -- Marble: M
   -- Stones: S

   putStrLn "Knowledge base: !(G && (S => M)) && !(!G && !S) && !(G && !M)"

   let kb4 = "!(G && (S => M)) && !(!G && !S) && !(G && !M)"   
   
   putStrLn "Query: Gold (G)"
   let isGold1 = inference (ttEntails (parse kb4) (parse "G")) (ttEntails (parse kb4) (parse "!G"))
   let isGold2 = inference (plResolution (parse kb4) (parse "G")) (plResolution (parse kb4) (parse "!G"))

   let test6 = "test6" ~: "Query Gold" ~: isGold2 ~=? isGold1
   
   putStrLn $ "Ans with model checking: " ++ isGold1
   putStrLn $ "Ans with resolution: " ++ isGold2
 
   putStrLn "Query: Marble (M)"
   let isMarble1 = inference (ttEntails (parse kb4) (parse "M")) (ttEntails (parse kb4) (parse "!M"))
   let isMarble2 = inference (plResolution (parse kb4) (parse "M")) (plResolution (parse kb4) (parse "!M"))

   let test7 = "test7" ~: "Query Marble" ~: isMarble2 ~=? isMarble1

   putStrLn $ "Ans with model checking: " ++ isMarble1
   putStrLn $ "Ans with resolution: " ++ isMarble2

   putStrLn "Query: Stones (S)"
   let isStones1 = inference (ttEntails (parse kb4) (parse "S")) (ttEntails (parse kb4) (parse "!S"))
   let isStones2 = inference (plResolution (parse kb4) (parse "S")) (plResolution (parse kb4) (parse "!S"))

   let test8 = "test8" ~: "Query Stones" ~: isStones2 ~=? isStones1

   putStrLn $ "Ans with model checking: " ++ isStones1
   putStrLn $ "Ans with resolution: " ++ isStones2 
     
   putStrLn ""  
   runTestTT $ test [test1, test3, test4, test5, test6, test7, test8]

