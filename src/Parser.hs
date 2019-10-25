module Parser (parse) where

import Formula
import Scanner

type TokenIterator = (Prop, [Token])
type TokenList = [Token]
type ExpressionStack = [Prop]
type OperatorStack = [String]
type ExpectedTokens = [String]


-- Helper function to add a node to the tree according to the precedence of operators
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

-- Helper function to add a new node to the tree
newProp :: ExpressionStack -> OperatorStack -> Prop -> Prop
newProp tree ("!":res) t = newProp tree res (Not t)
newProp _ [] t = (t)
newProp (tree:[]) ("&":[]) t = applyPrecedence tree "&" t
newProp (tree:[]) ("|":[]) t = applyPrecedence tree "|" t
newProp (tree:[]) ("=>":[]) t = applyPrecedence tree "=>" t
newProp (tree:[]) ("<=>":[]) t = applyPrecedence tree "<=>" t

-- Takes in a token list and parses it to return a tree representation
buildTree :: TokenList -> ExpressionStack -> OperatorStack -> ExpectedTokens -> TokenIterator
buildTree [] treeList op valid
    | "EOF" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = let (tree:[]) = treeList in (tree, [])
buildTree ((Param "(") : rest) treeList op valid
    | "(" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | op == [] = buildTree r [t] [] [")", "&", "|", "=>", "<=>", "EOF"]
    | otherwise = buildTree r [newProp treeList op t] [] [")", "&", "|", "=>", "<=>", "EOF"]
    where (t, r) = buildTree rest [] [] ["(", "!", "var"]
buildTree ((Param ")") : rest) treeList _ valid
    | ")" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = let (tree:[]) = treeList in (Par tree, rest)
buildTree ((Connective "<=>") : rest) treeList op valid
    | "<=>" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = buildTree rest treeList ["<=>"] ["(", "!", "var"]
buildTree ((Connective "=>") : rest) treeList op valid
    | "=>" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = buildTree rest treeList ["=>"] ["(", "!", "var"]
buildTree ((Connective "|") : rest) treeList op valid
    | "|" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = buildTree rest treeList ["|"] ["(", "!", "var"]
buildTree ((Connective "&") : rest) treeList op valid
    | "&" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = buildTree rest treeList ["&"] ["(", "!", "var"]
buildTree ((Connective "!") : rest) treeList op valid
    | "!" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | otherwise = buildTree rest treeList ("!" : op) ["(", "!", "var"]
buildTree ((Var s) : rest) treeList op valid
    | "var" `notElem` valid = errorWithoutStackTrace "Parse Error!"
    | op == [] = buildTree rest [(Atom s)] [] [")", "&", "|", "=>", "<=>", "EOF"] 
    | otherwise = buildTree rest [newProp treeList op (Atom s)] [] [")", "&", "|", "=>", "<=>", "EOF"]

-- Takes in a string and returns a tree representation of the formula
parse :: String -> Prop
parse formula = let (tree, _) = (buildTree (tokenize formula) [] [] ["(", "!", "var", "EOF"]) in (removeParams tree)
    where removeParams (Par l) = removeParams l
          removeParams (l :<=>: r) = (removeParams l) :<=>: (removeParams r)
          removeParams (l :=>: r) = (removeParams l) :=>: (removeParams r)
          removeParams (l :|: r) = (removeParams l) :|: (removeParams r)
          removeParams (l :&: r) = (removeParams l) :&: (removeParams r)
          removeParams (Not l) = Not (removeParams l)
          removeParams (Atom a) = Atom a


