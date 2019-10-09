{- CSC 442: Intro to AI
 - Spring 2019
 - Project 2: Automated Reasoning
 - Author: Soubhk Ghosh (netid: sghosh13)
-}

import Reasoning


main = do
   putStrLn "Modus Ponens test."
   putStrLn "Knowledge base:"
   putStrLn "P => Q"
   putStrLn "P"
   putStrLn "Query: Q"

   let kb1 = ((Symbol "P") :=>: (Symbol "Q")) :&&: (Symbol "P")
   let alpha1 = (Symbol "Q")
   let result1 = (ttEntails kb1 alpha1)
   putStrLn $ "Ans with model checking: " ++ (show result1) 

   putStrLn "Wumpus World test."

   let kb2 = (Not (Symbol "P11")) :&&: 
             ((Symbol "B11") :<=>: ((Symbol "P12") :||: (Symbol "P21"))) :&&: 
             ((Symbol "B21") :<=>: ((Symbol "P11") :||: (Symbol "P22") :||: (Symbol "P31"))) :&&:
             (Not (Symbol "B11")) :&&:
             (Symbol "B21")
   let alpha2 = (Symbol "P12")
   let result2 = (ttEntails kb2 alpha2)
   putStrLn $ "Ans with model checking: " ++ (show result2)
      

