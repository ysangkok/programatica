module TestData
  ( getTypings
  , genData
  , genExp'
  , subst
  , typeExp2Data
  , recover
  , hasGen
  , lookUp_pn
  , flatStr
  , gen_with_name
  , getLocEnv
  , fullTyp
  ) where
import UAbstract 
import UAnnots
import Random
import QuickCheck2
import ToAlfaExp
import GenNat --(Nat, arbNat, arbListNat, BinTree, arbBinTree, g, cmodel) 
import Natural

ePi2EAbs :: Exp -> Exp 
ePi2EAbs (EPi c e) = EAbs c (ePi2EAbs e)
ePi2EAbs e = e
 
getTypings :: Exp -> Typings -- local context of a hole 
getTypings (EPi c cs) = c : getTypings cs
getTypings _   = []

rndExp :: StdGen -> Exp
rndExp rnd = alfaExp (i2n (mod (fst (next rnd)) 13))

subst :: Exp -> [(String, Exp)] -> Exp
subst e [] = e
subst e (e1:es) = subst(subst1 e e1) es
subst1 :: Exp -> (String, Exp) -> Exp
subst1 (EVar (Var n)) (m, e) = if m == n then e else (EVar (Var n))
subst1 (EApp e1 e2) es = EApp (subst1 e1 es)(subst1 e2 es)
subst1 (EPi ((Var n) :- e1) e2) (m, e) = if m == n then (EPi ((Var n) :- e1) e2) else (EPi ((Var n) :- (subst1 e1 (m,e)))(subst1 e2 (m,e)))
subst1 (EProj e1 e2) e = EProj (subst1 e1 e) e2
subst1 e _ = e
 

typeExp2Data :: Exp -> StdGen -> Int -> (Exp, String)

typeExp2Data (EVar (Var "StdGen")) rnd size = 
    let s1 = mod size 11
        v = generate size rnd arbitrary::BinTree Natural
    in (alfaExp v, show "StdGen...") -- show v)

typeExp2Data (EVar (Var "StdGen'")) rnd size = 
     let s1 = mod size 11 
         v = generate size rnd arbitrary::BinTree Nat -- tree s1 rnd -- inf_tree wouldn't work
     in (alfaExp v, show "StdGen' ...") -- v)
typeExp2Data (EVar (Var "Binary")) rnd size = 
     let v = generate size rnd arbitrary::Natural
     in (alfaExp v, show v)
typeExp2Data (EVar (Var "Nat")) rnd size = 
     let v = generate size rnd arbitrary::Nat         
     in (alfaExp v, show v)

typeExp2Data (EApp (EVar (Var "List")) (EVar (Var "Nat"))) rnd size = 
     let v = generate size rnd arbitrary::[Nat]
     in (alfaExp v, show v) 
typeExp2Data (EVar (Var "Bool")) rnd size = 
     let v = generate size rnd arbitrary::Bool
     in (alfaExp v, show v)

typeExp2Data (EApp (EVar (Var "Gen")) (EVar (Var "Nat"))) rnd size =
     let v = generate size rnd arbitrary::Nat
     in (alfaExp v, show v)
typeExp2Data (EApp (EVar (Var "T")) e') rnd size =
     let e1 = EApp (EVar (Var "T")) e'
     in (e1, show e1)
typeExp2Data (EVar (Var tyC1)) rnd size = 
   let size1 = mod size 11
       es =  generate size rnd arbitrary::BinTree Nat 
       e = EApp (EVar (Var ("gen"++tyC1))) (alfaExp es)
   in (e, show e)
typeExp2Data (EApp (EVar (Var tyC)) e1) rnd size = 
   let size1 = mod size 11
       es =  generate size rnd arbitrary::BinTree Nat 
       e = EApp  (EApp (EVar (Var ("gen"++tyC))) e1) (alfaExp es) 
   in (e, show e)

typeExp2Data (EApp (EApp (EVar (Var tyC)) e1) e2) rnd size = 
   let size1 = mod size 11
       es =  generate size rnd arbitrary::BinTree Nat 
       e = EApp(EApp  (EApp (EVar (Var ("gen"++tyC))) e1) e2) (alfaExp es) 
   in (e, show e)
typeExp2Data e'@(EPi e1 e2) rnd size = 
   let size1 = mod size 11
       es =  generate size rnd arbitrary::BinTree Nat 
       na = getGFs e' ""
       e = EApp (EVar (Var ("genF"++na))) (alfaExp es) 
   in (e, show e)

typeExp2Data e rnd size = (e, show e)


-- generate a group of test data 
genData :: Typings -> StdGen -> Int -> [(String, (Exp, String))]
genData [] rnd0  size = []
genData ((x :- t):ts) rnd0 size = (varNames x, typeExp2Data t rnd0 size) : genData ts (snd(next rnd0)) size

 
-- instantiated expression to be computed
genExp' :: Exp -> [Exp] -> Exp
genExp' e es = app (ePi2EAbs e) es


varNames :: Var -> String
varNames (Var s) = s
-- varNames _ = []

--getLocEnv ::[(String, Exp, Maybe Exp)] -> [(String, Exp, Maybe Exp)]
getLocEnv []                     = []
getLocEnv ((_, EPi e1 e2, _):es) = []
getLocEnv (e:es)                 = e: getLocEnv es


subst' :: Exp -> [(String, Exp)] -> Exp
subst' e [] = e
subst' e (e1:es) = subst'(subst1' e e1) es
subst1' :: Exp -> (String, Exp) -> Exp
subst1' (EVar (Var n)) (m, e) = if m == n then e else (EVar (Var n))
subst1' (EApp e1 e2) es = EApp (subst1' e1 es)(subst1' e2 es)
subst1' (EPi ((Var n) :- e1) e2) (m, e) = (EPi ((Var n) :- (subst1' e1 (m,e)))(subst1' e2 (m,e)))
subst1' (EProj e1 e2) e = EProj (subst1' e1 e) e2
subst1' e _ = e
 

-- while in a hole, get the parameters (one the left side of :: int the type expression)
-- getPars :: Exp -> [(String, Exp, Maybe Exp)] -> [(String, Exp, Maybe Exp)]
getPars e es = 
  let subs = [(v, e) | (v, _, Just e) <- es]
      
  in takeWhile (not.(isTail e).(\ z -> subst' z subs).snd) [(v, x) | (v, x, Nothing) <- es]

-- abstract the parameters
-- abstractPars :: Exp -> [(String, Exp)] -> Exp
abstractPars e [] = e
abstractPars e ((v, e1):es) = abstractPars (EPi (Var v :- e1) e) es


-- from the type exp on the right of ::, and the context (qiaoContext), get a full abstraction    
fullTyp:: Exp ->  [(String, Exp, Maybe Exp)] -> Exp

fullTyp e es = simplify$ abstractPars e (getPars e es)

simplify ::Exp -> Exp
simplify (EPi (e1 :- EProj _ (Label t)) e2) = EPi (e1 :- EVar (Var t)) (simplify e2)
simplify (EPi (e1 :- t) e2) = EPi (e1 :- t) (simplify e2)
simplify e = e
  

isTail :: Exp -> Exp -> Bool
isTail e e' = e == e' || isTail' e e'

isTail' e (EPi _ e1) 
   | e == e1    = True
   | otherwise  = isTail' e e1
isTail' e _ = False


recover ::([(Var, Exp)], Exp) -> Exp
recover ([], ex) = ex
recover ((vn, EVar t):es, e) = recover(es, EPi (vn :- EVar t) e)
recover ((vn, EApp t1 t2):es, e) = recover(es, EPi (vn :- EApp t1 t2) e)
recover ((vn, EProj t1 (Label ty)):es, e) = recover(es, EPi (vn :- EVar (Var ty)) e)
recover ((vn, EPi t1 t2):es, e)  = e
recover  (_, e) = e

hasGen1 :: String -> String ->  [(String, Exp, Maybe Exp)]  -> Bool
hasGen1 gen ty es  =  (gen ++ ty) `elem` [s | (s, _ , _) <- es]

gen_with_name :: String -> StdGen -> Int -> Exp
gen_with_name na rnd size = 
   let size1 = mod size 11 
       es =  generate size rnd arbitrary::BinTree Nat
   in  EApp (EVar (Var na)) (alfaExp es) 

--hasGen :: Exp -> [(String, Exp, Maybe Exp)] -> Bool
hasGen  (EVar (Var "StdGen")) _ = True
hasGen  (EVar (Var "Nat")) _ = True
hasGen  (EVar (Var "Binary")) _ = True
hasGen  (EVar (Var "Bool")) _ = True
hasGen  (EVar (Var "StdGen'")) _ = True
hasGen (EApp (EVar (Var "List")) (EVar (Var "Nat"))) _ = True
hasGen  (EVar (Var tyC)) es = hasGen1 "gen" tyC es
hasGen (EApp (EVar (Var tyC)) e1) es = hasGen1 "gen" tyC es
hasGen (EApp (EApp (EVar (Var tyC)) e1) e2) es = hasGen1 "gen" tyC es
hasGen e@(EPi e1 e2) es = 
  let a = getGFs e ""
  in hasGen1 "genF" a es 
hasGen _ _ = False

getGFs (EVar (Var na)) n0 = n0++na
getGFs (EPi (_ :- e1) e2) n0 = getGFs e2 (getGFs e1 n0)
getGFs _ n0 = n0

lookUp_pn :: [(Var, Exp)] -> String
lookUp_pn ((Var vn,  EPi t1 t2):es) = vn
lookUp_pn (_:es) = lookUp_pn es
lookUp_pn _ = []


flatStr :: Exp -> [(Var, Exp)]
flatStr (EStr ds) = [v |Decl _ defs <- ds,  DefA _ (Binding v) <- defs ]
flatStr _ = []
