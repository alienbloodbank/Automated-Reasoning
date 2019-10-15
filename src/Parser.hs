module Parser (parse) where

import Formula
import Scanner

-- !, &&, ||, =>, <=>
applyPrecedence :: Prop -> String -> Prop -> Prop
applyPrecedence (Par l) op t
    | op == "&&" = (Par l) :&&: (t)
    | op == "||" = (Par l) :||: (t)
    | op == "=>" = (Par l) :=>: (t)
    | op == "<=>" = (Par l) :<=>: (t)
applyPrecedence (Symbol l) op t
    | op == "&&" = (Symbol l) :&&: (t)
    | op == "||" = (Symbol l) :||: (t)
    | op == "=>" = (Symbol l) :=>: (t)
    | op == "<=>" = (Symbol l) :<=>: (t)
applyPrecedence (Not l) op t
    | op == "&&" = (Not l) :&&: (t)
    | op == "||" = (Not l) :||: (t)
    | op == "=>" = (Not l) :=>: (t)
    | op == "<=>" = (Not l) :<=>: (t)
applyPrecedence (l :<=>: r) op t
    | (op == "&&") || (op == "||") || (op == "=>") = l :<=>: (applyPrecedence r op t)
    | op == "<=>" = (l :<=>: r) :<=>: (t)
applyPrecedence (l :=>: r) op t
    | (op == "&&") || (op == "||") = l :=>: (applyPrecedence r op t)
    | op == "<=>" = (l :=>: r) :<=>: (t)
    | op == "=>" = (l :=>: r) :=>: (t)
applyPrecedence (l :||: r) op t
    | op == "&&" = l :||: (applyPrecedence r "&&" t)
    | op == "||" = (l :||: r) :||: (t)
    | op == "=>" = (l :||: r) :=>: (t)
    | op == "<=>" = (l :||: r) :<=>: (t)
applyPrecedence (l :&&: r) op t
    | op == "&&" = (l :&&: r) :&&: (t)
    | op == "||" = (l :&&: r) :||: (t)
    | op == "=>" = (l :&&: r) :=>: (t)
    | op == "<=>" = (l :&&: r) :<=>: (t)


newProp :: [Prop] -> [String] -> Prop -> Prop
newProp temp ("!":res) t = newProp temp res (Not t)
newProp _ [] t = (t)
newProp (temp:[]) ("&&":[]) t = applyPrecedence temp "&&" t
newProp (temp:[]) ("||":[]) t = applyPrecedence temp "||" t
newProp (temp:[]) ("=>":[]) t = applyPrecedence temp "=>" t
newProp (temp:[]) ("<=>":[]) t = applyPrecedence temp "<=>" t


buildTree :: [Token] -> [Prop] -> [String] -> (Prop, [Token])
buildTree [] sym _ = let (temp:[]) = sym in (temp, [])
buildTree ((Param "(") : rest) sym op
    | op == [] = buildTree r [t] []
    | otherwise = buildTree r [newProp sym op t] []
    where (t, r) = (buildTree rest [] [])
buildTree ((Param ")") : rest) sym _ = let (temp:[]) = sym in (Par temp, rest)
buildTree ((Connective "<=>") : rest) sym op = buildTree rest sym ["<=>"]
buildTree ((Connective "=>") : rest) sym op = buildTree rest sym ["=>"]
buildTree ((Connective "||") : rest) sym op = buildTree rest sym ["||"]
buildTree ((Connective "&&") : rest) sym op = buildTree rest sym ["&&"]
buildTree ((Connective "!") : rest) sym op = buildTree rest sym ("!":op)
buildTree ((Var s) : rest) sym op
    | op == [] = buildTree rest [(Symbol s)] []
    | otherwise = buildTree rest [newProp sym op (Symbol s)] []


removeParams :: Prop -> Prop
removeParams (Par l) = removeParams l
removeParams (l :<=>: r) = (removeParams l) :<=>: (removeParams r)
removeParams (l :=>: r) = (removeParams l) :=>: (removeParams r)
removeParams (l :||: r) = (removeParams l) :||: (removeParams r)
removeParams (l :&&: r) = (removeParams l) :&&: (removeParams r)
removeParams (Not l) = Not (removeParams l)
removeParams (Symbol s) = Symbol s


parse :: String -> Prop
parse formula = let (tree, _) = (buildTree (tokenize formula) [] []) in (removeParams tree)


