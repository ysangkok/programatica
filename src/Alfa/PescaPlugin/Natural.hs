module Natural where

import PrelSequent
import Sequent

type Hypotheses = [(Int,Formula)]

data NatProof =
   Hypo    Int Formula
 | Unknown Ident Formula
 | ConjI  Formula Formula NatProof NatProof
 | ConjE  Formula Formula Formula NatProof Int Int NatProof
 | DisjIl Formula Formula NatProof 
 | DisjIr Formula Formula NatProof
 | DisjE  Formula Formula Formula NatProof Int NatProof Int NatProof
 | ImplI  Formula Formula Int NatProof
 | ImplE  Formula Formula Formula NatProof NatProof Int NatProof
 | UnivI  Formula Ident NatProof
 | UnivE  Formula Formula Formula NatProof Int Int NatProof
 | ExistI Formula Formula Ident NatProof
 | ExistE Formula Formula NatProof Int NatProof
 | FalsE  Formula NatProof

 | MP     Formula Formula NatProof NatProof -- added 7/11/2000

proof2nat = proof2natproof []

proof2natproof :: Hypotheses -> Proof -> NatProof
proof2natproof hypo proof =
 case proof of
   Proof rule sequent proofs ->
     case (rule,sequent,proofs) of
       ("ax",  (_,         [a]),         []) ->
           findHypo hypo a
       ("R&",  (_,         [Conj a b]),  [prem1,prem2]) -> 
           ConjI a b (pn prem1) (pn prem2)
       ("L&",  (Conj a b:_,[c]),         [prem1]) -> 
           ConjE a b c mainprem h1 (h1 + 1) (pn' [a,b] prem1)
             where
               mainprem = findHypo hypo (Conj a b)
               h1       = maximum' (map fst hypo) + 1
       ("Rv1", (_,         [Disj a b]),  [prem1]) -> 
           DisjIl a b (pn prem1)
       ("Rv2", (_,         [Disj a b]),  [prem1]) -> 
           DisjIr a b (pn prem1)
       ("Lv",  (Disj a b:_,[c]),         [prem1,prem2]) -> 
           DisjE a b c mainprem h1 (pn' [a] prem1) h2 (pn' [b] prem2)
             where
               mainprem = findHypo hypo (Disj a b)
               h1       = maximum' (map fst hypo) + 1
               h2       = h1 --- + 1
       ("R->", (_,         [Impl a b]), [prem1]) ->
           ImplI a b h1 (pn' [a] prem1)
             where
               h1       = maximum' (map fst hypo) + 1
       ("R->", (_,         [Neg a]), [prem1]) ->
           ImplI a Falsum h1 (pn' [a] prem1)
             where
               h1       = maximum' (map fst hypo) + 1
       ("R/A", (_,         [Univ x a]),  [prem1]) ->
           UnivI a x (pn prem1)
       ("R/E", (_,         [Exist x a]), [prem1]) ->
           ExistI a a' x (pn prem1)
             where
               a'       = leftFormOf prem1

       ("L->", (Impl a b:_,[c]),         [prem1,prem2]) -> 
           ImplE a b c mainprem (pn prem1) h1 (pn' [b] prem2)
             where
               mainprem = findHypo hypo (Impl a b)
               h1       = maximum' (map fst hypo) + 1


       ("L->", (Neg a:_,[c]),         [prem1,prem2]) -> 
           ImplE a Falsum c mainprem (pn prem1) h1 (pn' [Falsum] prem2)
             where
               mainprem = findHypo hypo (Impl a Falsum)
               h1       = maximum' (map fst hypo) + 1
       ("L_|_", (Falsum:_, [c]),[]) -> 
           FalsE c mainprem
             where
               mainprem = findHypo hypo Falsum
       ("L/A", (Univ x a:_,[c]),         [prem1]) -> 
           UnivE a a' c mainprem h1 (h1 + 1) (pn' [a', Univ x a] prem1)
             where
               a'       = leftFormOf prem1
               mainprem = findHypo hypo (Univ x a)
               h1       = maximum' (map fst hypo) + 1
       ("L/E", (Exist x a:_,[c]),         [prem1]) -> 
           ExistE a c mainprem h1 (pn' [a] prem1)
             where
               mainprem = findHypo hypo (Exist x a)
               h1       = maximum' (map fst hypo) + 1
       _ -> Unknown "undef" (Scheme "?" [])


   _ -> Unknown "undef" (Scheme "?" [])
  where 
     pn           = proof2natproof hypo
     pn' formulae = proof2natproof (freshHypos formulae hypo)
     leftFormOf p = case p of
                      Goal    (a:_,_)   -> a
                      Proof _ (a:_,_) _ -> a
                      _                 -> Scheme "\\mbox{ass.}" []

maximum' []  = 0
maximum' l   = maximum l 

freshHypos :: [Formula] -> Hypotheses -> Hypotheses
freshHypos formulae hypo = zip [nxt ..] formulae ++ hypo where
 nxt = maximum' (map fst hypo) + 1

findHypo :: Hypotheses -> Formula -> NatProof
findHypo hypo formula =
 case lookup formula [(f,n) | (n,f) <- hypo] of
   Just n -> Hypo n formula
   _      -> case formula of
               Neg a         -> findHypo hypo (Impl a Falsum)
               Impl a Falsum -> findHypo hypo (Neg a) 
               _             -> Unknown "\\mbox{ass.}" formula

