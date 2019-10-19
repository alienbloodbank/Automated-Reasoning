module CNF_converter (genCNF, applyDeMorgan) where

import Formula

-- Accepts any WFF
eliminateBicond :: Prop -> Prop
eliminateBicond (Atom a) = (Atom a)
eliminateBicond (Not t) = (Not (eliminateBicond t))
eliminateBicond (l :<=>: r) = ((eliminateBicond l) :=>: (eliminateBicond r)) :&: ((eliminateBicond r) :=>: (eliminateBicond l))
eliminateBicond (l :=>: r) = (eliminateBicond l) :=>: (eliminateBicond r)
eliminateBicond (l :|: r) = (eliminateBicond l) :|: (eliminateBicond r)
eliminateBicond (l :&: r) = (eliminateBicond l) :&: (eliminateBicond r)

-- Assumes <=> is eliminated
eliminateImplies :: Prop -> Prop
eliminateImplies (Atom a) = (Atom a)
eliminateImplies (Not t) = (Not (eliminateImplies t))
eliminateImplies (l :=>: r) = (Not (eliminateImplies l)) :|: (eliminateImplies r)
eliminateImplies (l :|: r) = (eliminateImplies l) :|: (eliminateImplies r)
eliminateImplies (l :&: r) = (eliminateImplies l) :&: (eliminateImplies r)

-- Assumes <=>, => is eliminated
deMorgan :: Prop -> Bool -> Prop
deMorgan (Atom a) True = (Atom a)
deMorgan (Atom a) False = Not (Atom a)
deMorgan (Not (Atom a)) True = Not (Atom a)
deMorgan (Not (Atom a)) False = (Atom a)
deMorgan (Not t) True = deMorgan t False
deMorgan (Not t) False = deMorgan t True
deMorgan (l :|: r) True = (deMorgan l True) :|: (deMorgan r True)
deMorgan (l :|: r) False = (deMorgan l False) :&: (deMorgan r False)
deMorgan (l :&: r) True = (deMorgan l True) :&: (deMorgan r True)
deMorgan (l :&: r) False = (deMorgan l False) :|: (deMorgan r False)

applyDeMorgan :: Prop -> Prop
applyDeMorgan p = deMorgan p True

applyOr :: Prop -> Prop -> Prop
applyOr (Atom p) (Atom q) = (Atom p) :|: (Atom q)
applyOr (Not p) (Atom q) = (Not p) :|: (Atom q)
applyOr (Atom p) (Not q) = (Atom p) :|: (Not q)
applyOr (Not p) (Not q) = (Not p) :|: (Not q)
applyOr (Atom s) r = applyOr r (Atom s)
applyOr (Not t) r = applyOr r (Not t)
applyOr (p :|: q) (Atom s) = p :|: q :|: (Atom s)
applyOr (p :|: q) (Not t) = p :|: q :|: (Not t)
applyOr (p :|: q) (r :|: s) = p :|: q :|: r :|: s
applyOr (p :|: q) r = applyOr r (p :|: q)
applyOr (p :&: q) r = (applyOr p r) :&: (applyOr q r)

-- Assumes <=>, => is eliminated and ! is moved inwards
distributeOr :: Prop -> Prop
distributeOr (Atom a) = (Atom a)
distributeOr (Not t) = Not t
distributeOr (l :|: r) = applyOr (distributeOr l) (distributeOr r)
distributeOr (l :&: r) = (distributeOr l) :&: (distributeOr r)

genCNF :: Prop -> Prop
genCNF = distributeOr . applyDeMorgan . eliminateImplies . eliminateBicond
