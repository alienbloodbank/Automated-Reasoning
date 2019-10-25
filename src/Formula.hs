module Formula (Prop(..)) where

infixl 4 :&:
infixl 3 :|:
infixl 2 :=>:
infixl 1 :<=>:
data Prop = Atom String -- A WFF is an atom
         | Par Prop -- A WFF is a WFF inside paranthesis
         | Not Prop -- A WFF is a negation of a WFF
         | Prop :&: Prop -- A WFF is a conjunction of 2 WFFs
         | Prop :|: Prop -- A WFF is a disjunction of 2 WFFs
         | Prop :=>: Prop -- A WFF is an implication between 2 WFFs
         | Prop :<=>: Prop -- A WFF is a biconditional between 2 WFFs
           deriving (Eq, Ord)


instance Show Prop where
    show (f :<=>: g) = "(" ++ show f ++ " <=> " ++ show g ++ ")"
    show (f :=>: g) = "(" ++ show f ++ " => " ++ show g ++ ")"
    show (f :|: g) = "(" ++ show f ++ " | " ++ show g ++ ")"
    show (f :&: g) = "(" ++ show f ++ " & " ++ show g ++ ")"
    show (Not f) = "!(" ++ show f ++ ")"
    show (Atom p) = p

