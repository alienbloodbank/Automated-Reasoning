module CNF_converter (genCNF, applyDeMorgan) where

import Formula

-- Any WFF
eliminateBicond :: Prop -> Prop
eliminateBicond (Symbol s) = (Symbol s)
eliminateBicond (Not t) = (Not (eliminateBicond t))
eliminateBicond (l :<=>: r) = ((eliminateBicond l) :=>: (eliminateBicond r)) :&&: ((eliminateBicond r) :=>: (eliminateBicond l))
eliminateBicond (l :=>: r) = (eliminateBicond l) :=>: (eliminateBicond r)
eliminateBicond (l :||: r) = (eliminateBicond l) :||: (eliminateBicond r)
eliminateBicond (l :&&: r) = (eliminateBicond l) :&&: (eliminateBicond r)

-- Assumes <=> is eliminated
eliminateImplies :: Prop -> Prop
eliminateImplies (Symbol s) = (Symbol s)
eliminateImplies (Not t) = (Not (eliminateImplies t))
eliminateImplies (l :=>: r) = (Not (eliminateImplies l)) :||: (eliminateImplies r)
eliminateImplies (l :||: r) = (eliminateImplies l) :||: (eliminateImplies r)
eliminateImplies (l :&&: r) = (eliminateImplies l) :&&: (eliminateImplies r)

-- Assumes <=>, => is eliminated
deMorgan :: Prop -> Bool -> Prop
deMorgan (Symbol s) True = (Symbol s)
deMorgan (Symbol s) False = Not (Symbol s)
deMorgan (Not (Symbol s)) True = Not (Symbol s)
deMorgan (Not (Symbol s)) False = (Symbol s)
deMorgan (Not t) True = deMorgan t False
deMorgan (Not t) False = deMorgan t True
deMorgan (l :||: r) True = (deMorgan l True) :||: (deMorgan r True)
deMorgan (l :||: r) False = (deMorgan l False) :&&: (deMorgan r False)
deMorgan (l :&&: r) True = (deMorgan l True) :&&: (deMorgan r True)
deMorgan (l :&&: r) False = (deMorgan l False) :||: (deMorgan r False)

applyDeMorgan :: Prop -> Prop
applyDeMorgan p = deMorgan p True

applyOr :: Prop -> Prop -> Prop
applyOr (Symbol p) (Symbol q) = (Symbol p) :||: (Symbol q)
applyOr (Not p) (Symbol q) = (Not p) :||: (Symbol q)
applyOr (Symbol p) (Not q) = (Symbol p) :||: (Not q)
applyOr (Not p) (Not q) = (Not p) :||: (Not q)
applyOr (Symbol s) r = applyOr r (Symbol s)
applyOr (Not t) r = applyOr r (Not t)
applyOr (p :||: q) (Symbol s) = p :||: q :||: (Symbol s)
applyOr (p :||: q) (Not t) = p :||: q :||: (Not t)
applyOr (p :||: q) (r :||: s) = p :||: q :||: r :||: s
applyOr (p :||: q) r = applyOr r (p :||: q)
applyOr (p :&&: q) r = (applyOr p r) :&&: (applyOr q r)

-- Assumes <=>, => is eliminated and ! is moved inwards
distributeOr :: Prop -> Prop
distributeOr (Symbol s) = (Symbol s)
distributeOr (Not t) = Not t
distributeOr (l :||: r) = applyOr (distributeOr l) (distributeOr r)
distributeOr (l :&&: r) = (distributeOr l) :&&: (distributeOr r)

genCNF :: Prop -> Prop
genCNF = distributeOr . applyDeMorgan . eliminateImplies . eliminateBicond
