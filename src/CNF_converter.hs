module CNF_converter (genCNF, applyDeMorgan, literalsInClause, getCNFClauses) where

import Formula

-- Eliminates biconditional operators from a formula
-- Accepts any WFF
eliminateBicond :: Prop -> Prop
eliminateBicond (Atom a) = (Atom a)
eliminateBicond (Not t) = (Not (eliminateBicond t))
eliminateBicond (l :<=>: r) = ((eliminateBicond l) :=>: (eliminateBicond r)) :&: ((eliminateBicond r) :=>: (eliminateBicond l))
eliminateBicond (l :=>: r) = (eliminateBicond l) :=>: (eliminateBicond r)
eliminateBicond (l :|: r) = (eliminateBicond l) :|: (eliminateBicond r)
eliminateBicond (l :&: r) = (eliminateBicond l) :&: (eliminateBicond r)

-- Elimiates implication operators from a formula
-- Assumes <=> is eliminated
eliminateImplies :: Prop -> Prop
eliminateImplies (Atom a) = (Atom a)
eliminateImplies (Not t) = (Not (eliminateImplies t))
eliminateImplies (l :=>: r) = (Not (eliminateImplies l)) :|: (eliminateImplies r)
eliminateImplies (l :|: r) = (eliminateImplies l) :|: (eliminateImplies r)
eliminateImplies (l :&: r) = (eliminateImplies l) :&: (eliminateImplies r)

-- Applies De Morgan and moves negation inwards
-- Assumes <=>, => is eliminated
applyDeMorgan :: Prop -> Prop
applyDeMorgan p = deMorgan p True
    where deMorgan (Atom a) True = (Atom a)
          deMorgan (Atom a) False = Not (Atom a)
          deMorgan (Not (Atom a)) True = Not (Atom a)
          deMorgan (Not (Atom a)) False = (Atom a)
          deMorgan (Not t) True = deMorgan t False
          deMorgan (Not t) False = deMorgan t True
          deMorgan (l :|: r) True = (deMorgan l True) :|: (deMorgan r True)
          deMorgan (l :|: r) False = (deMorgan l False) :&: (deMorgan r False)
          deMorgan (l :&: r) True = (deMorgan l True) :&: (deMorgan r True)
          deMorgan (l :&: r) False = (deMorgan l False) :|: (deMorgan r False)

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

-- Distributes disjunctions over any conjunction
-- Assumes <=>, => is eliminated and ! is moved inwards
distributeOr :: Prop -> Prop
distributeOr (Atom a) = (Atom a)
distributeOr (Not t) = Not t
distributeOr (l :|: r) = applyOr (distributeOr l) (distributeOr r)
distributeOr (l :&: r) = (distributeOr l) :&: (distributeOr r)

-- Takes a formula tree and returns a CNF tree
genCNF :: Prop -> Prop
genCNF = distributeOr . applyDeMorgan . eliminateImplies . eliminateBicond

literalsInClause :: Prop -> [Prop]
literalsInClause (Atom p) = [Atom p]
literalsInClause (Not prop) = [Not prop]
literalsInClause (propl :|: propr) = (literalsInClause propl) ++ (literalsInClause propr)
literalsInClause _ = []

getCNFClauses :: Prop -> [Prop]
getCNFClauses (propl :&: propr) = (getCNFClauses propl) ++ (getCNFClauses propr)
getCNFClauses prop = [prop]

