module Parser (parse) where

import Formula
import Scanner

type TokenIterator = (Prop, [Token])
type TokenList = [Token]

-- !, &, |, =>, <=>
applyPrecedence :: Prop -> String -> Prop -> Prop
applyPrecedence n@(Par l) op t
    | op == "&" = n :&: t
    | op == "|" = n :|: t
    | op == "=>" = n :=>: t
    | op == "<=>" = n :<=>: t
applyPrecedence n@(Atom l) op t
    | op == "&" = n :&: t
    | op == "|" = n :|: t
    | op == "=>" = n :=>: t
    | op == "<=>" = n :<=>: t
applyPrecedence n@(Not l) op t
    | op == "&" = n :&: t
    | op == "|" = n :|: t
    | op == "=>" = n :=>: t
    | op == "<=>" = n :<=>: t
applyPrecedence n@(l :<=>: r) op t
    | (op == "&") || (op == "|") || (op == "=>") = l :<=>: (applyPrecedence r op t)
    | op == "<=>" = n :<=>: t
applyPrecedence n@(l :=>: r) op t
    | (op == "&") || (op == "|") = l :=>: (applyPrecedence r op t)
    | op == "<=>" = n :<=>: t
    | op == "=>" = n :=>: t
applyPrecedence n@(l :|: r) op t
    | op == "&" = l :|: (applyPrecedence r op t)
    | op == "|" = n :|: t
    | op == "=>" = n :=>: t
    | op == "<=>" = n :<=>: t
applyPrecedence n@(l :&: r) op t
    | op == "&" = n :&: t
    | op == "|" = n :|: t
    | op == "=>" = n :=>: t
    | op == "<=>" = n :<=>: t


newProp :: [Prop] -> [String] -> Prop -> Prop
newProp tree ("!":res) t = newProp tree res (Not t)
newProp _ [] t = (t)
newProp (tree:[]) ("&":[]) t = applyPrecedence tree "&" t
newProp (tree:[]) ("|":[]) t = applyPrecedence tree "|" t
newProp (tree:[]) ("=>":[]) t = applyPrecedence tree "=>" t
newProp (tree:[]) ("<=>":[]) t = applyPrecedence tree "<=>" t


buildTree :: TokenList -> [Prop] -> [String] -> TokenIterator
buildTree [] treeList _ = let (tree:[]) = treeList in (tree, [])
buildTree ((Param "(") : rest) treeList op
    | op == [] = buildTree r [t] []
    | otherwise = buildTree r [newProp treeList op t] []
    where (t, r) = (buildTree rest [] [])
buildTree ((Param ")") : rest) treeList _ = let (tree:[]) = treeList in (Par tree, rest)
buildTree ((Connective "<=>") : rest) treeList op = buildTree rest treeList ["<=>"]
buildTree ((Connective "=>") : rest) treeList op = buildTree rest treeList ["=>"]
buildTree ((Connective "|") : rest) treeList op = buildTree rest treeList ["|"]
buildTree ((Connective "&") : rest) treeList op = buildTree rest treeList ["&"]
buildTree ((Connective "!") : rest) treeList op = buildTree rest treeList ("!" : op)
buildTree ((Var s) : rest) treeList op
    | op == [] = buildTree rest [(Atom s)] []
    | otherwise = buildTree rest [newProp treeList op (Atom s)] []


removeParams :: Prop -> Prop
removeParams (Par l) = removeParams l
removeParams (l :<=>: r) = (removeParams l) :<=>: (removeParams r)
removeParams (l :=>: r) = (removeParams l) :=>: (removeParams r)
removeParams (l :|: r) = (removeParams l) :|: (removeParams r)
removeParams (l :&: r) = (removeParams l) :&: (removeParams r)
removeParams (Not l) = Not (removeParams l)
removeParams (Atom a) = Atom a


parse :: String -> Prop
parse formula = let (tree, _) = (buildTree (tokenize formula) [] []) in (removeParams tree)

