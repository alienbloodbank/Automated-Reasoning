module Formula (Prop(..)) where

infixl 4 :&:
infixl 3 :|:
infixl 2 :=>:
infixl 1 :<=>:
data Prop = Atom String
         | Par Prop
         | Not Prop
         | Prop :&: Prop
         | Prop :|: Prop
         | Prop :=>: Prop
         | Prop :<=>: Prop deriving (Eq, Ord)


instance Show Prop where
    show (f :<=>: g) = "(" ++ show f ++ " <=> " ++ show g ++ ")"
    show (f :=>: g) = "(" ++ show f ++ " => " ++ show g ++ ")"
    show (f :|: g) = "(" ++ show f ++ " || " ++ show g ++ ")"
    show (f :&: g) = show f ++ " && " ++ show g
    show (Not f) = "!(" ++ show f ++ ")"
    show (Atom p) = p

