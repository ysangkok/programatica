module PescaAlfa where

import UAbstract
import AbstractOps

import qualified Sequent as S
import Natural 
import PrelSequent ((+++),(++++),(***))
import Interaction (tryMethod)
import Calculi (Calculus(..),rulesOfCalculus)
import PSequent

-- global constants
propCalc :: S.AbsCalculus
propCalc = rulesOfCalculus (Calculus ["G3ip"]) --- change to G4ip

depthSearch :: Int
depthSearch = 8 --- could be user-definable

-------------------------------------------------------------------------
-- The main methods

-- try to find a proof term for the goal prop.
-- If successful, return this term. Otherwise return the goal back

tryProveAlfa :: Int -> Exp -> Exp -> Exp
tryProveAlfa d goal prop = case tryMethod propCalc d (alfa2sequent prop) of
  proof:_ -> proof2alfa (proof2nat proof)
  _ -> goal

-- parse a formula using Pesca parser

parseFormula :: String -> [(Exp,String)]
parseFormula = pFormula 1 *** formula2alfa

-- end of the main methods
-------------------------------------------------------------------------

-- the command we would like to have:
--   choose a metavariable goal :: prop :: Prop
--   click "try sequent calculus"
--   replace goal by the value of tryProveAlfa goal prop

-- notice: names of Satslogik are hard-wired in the method.
-------------------------------------------------------------------------

-- this is how it is done:

alfa2sequent :: Exp -> S.Sequent
alfa2sequent prop = ([],[alfa2formula prop])

alfa2formula :: Exp -> S.Formula
alfa2formula prop = case flatApp' prop of
  (EVar (Var c), [x1,x2]) -> case c of
    "And"         -> S.Conj (a2f x1) (a2f x2)
    "Or"          -> S.Disj (a2f x1) (a2f x2)
    "Implies"     -> S.Impl (a2f x1) (a2f x2)
    "Equivalence" -> let (y1,y2) = (a2f x1,a2f x2) in 
                     S.Conj (S.Impl y1 y2) (S.Impl y2 y1)
    _ -> S.Other prop
  (EVar (Var "Not"), [x]) -> S.Neg (a2f x)
  (EVar (Var "Absurdity"), []) -> S.Falsum
  (EVar (Var c), []) -> S.Scheme c []
  _ -> S.Other prop
 where
   a2f = alfa2formula

proof2alfa :: NatProof -> Exp
proof2alfa p = case p of
  Hypo x aa -> 
    EProofOf (f2a aa) (EVar (mkHypoVar x))
  Unknown c a -> eMeta

  ConjI aa bb a b -> 
    let (aa',bb') = (f2a aa, f2a bb) in
    appcsp (appcs "And" [aa', bb']) "AndIntro" [aa', bb', p2a a, p2a b]
  ConjE aa bb cc c x y d -> 
    let (aa',bb', cc') = (f2a aa, f2a bb, f2a cc) in
    appcsp cc' 
      "AndGenElim" [aa', bb', cc', p2a c, absc [(x,aa'),(y,bb')] (p2a d)]
  DisjIl aa bb a -> 
    let (aa',bb') = (f2a aa, f2a bb) in
    appcsp (appcs "Or" [aa', bb']) "OrIntro1" [aa', bb', p2a a]
  DisjIr aa bb b -> 
    let (aa',bb') = (f2a aa, f2a bb) in
    appcsp (appcs "Or" [aa', bb']) "OrIntro2" [aa', bb', p2a b]
  DisjE aa bb cc c x d y e -> 
    let (aa',bb', cc') = (f2a aa, f2a bb, f2a cc) in
    appcsp cc' "OrElim" [aa', bb', cc', p2a c, 
                         absc [(x,aa')] (p2a d), absc [(y,bb')] (p2a e)]
  ImplI aa bb x b -> 
    let (aa',bb') = (f2a aa, f2a bb) in
    appcsp (appcs "Implies" [aa', bb']) 
           "ImpliesIntro" [aa', bb', absc [(x,aa')] (p2a b)]
  ImplE aa bb cc c d x e -> 
    let (aa',bb',cc') = (f2a aa, f2a bb, f2a cc) in
    appcsp cc' 
      "ImpliesGenElim" [aa', bb', cc', p2a c, p2a d, absc [(x,bb')] (p2a e)]
  FalsE cc c ->
    let cc' = f2a cc in 
    appcsp cc' "AbsurdityElim" [cc', p2a c]
 where
   f2a = formula2alfa
   p2a = proof2alfa

formula2alfa :: S.Formula -> Exp
formula2alfa f = case f of
  S.Scheme c [] -> appcs c []
  S.Falsum      -> appcs "Absurdity" []
  S.Neg a       -> appcs "Not"     [f2a a]
  S.Conj a b    -> appcs "And"     [f2a a, f2a b]
  S.Disj a b    -> appcs "Or"      [f2a a, f2a b]
  S.Impl a b    -> appcs "Implies" [f2a a, f2a b]
  _             -> eMeta
 where
   f2a = formula2alfa


appcs c xx = app (EVar (Var c)) xx
appcsp t c xx = EProofOf t (appcs c xx)

absc xx b = foldr mkHyp b xx where
  mkHyp (i,a) b = EAbs (mkHypoVar i :- a) b

acons c = EVar (Var c)

mkHypoVar i = Var ('x':show i) ---

eMeta = ePreMetaVar

-- printer for Alfa
pra (EApp f a) = "(" ++ pra f +++ pra a ++ ")"
pra (EAbs (Var x :- a) b) = "(\\" ++ x ++ "::" ++ pra a ++++ "->" ++ pra b ++ ")"
pra (EVar (Var c)) = c
pra (EProofOf a b) = pra b --- 
