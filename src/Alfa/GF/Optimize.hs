module Optimize where

import Operations
import Tokens
import Grammar
import Macros
import SymbolTable
import ComputeTerm
import Lookup
import Rename
import LinTypes
import Linearize (lterm2alts)
import PrGrammar

-- make linearization faster by precomputing rules. AR 7/3/2000 -- 29/3

--- optimizeLInfo :: Token a => GrammarST a -> 
---                                 (Ident,IdentLInfo a) -> Err (Ident,IdentLInfo a)
optimizeLInfo st@(ab,cnc) ii@(ident, linfo) = errIn (prt ident) $ case linfo of
  IdentLin xx0 t0 -> do
    ltyp <- linTypeOfFun ident st
    (cont,vtyp) <- ltypeForm ltyp
    mkOptLin (map snd cont) vtyp xx0 t0 IdentLin
  IdentDefault xx0 t0 -> do
    let cat = stripAdHocIdent ident
    cont0 <- lookupCat cat ab
    cont1 <- mapM (valCat . snd) cont0
    cont  <- mapM (flip lookupLinTypeOfCat cnc) cont1
    vtyp  <- lookupLinTypeOfCat cat cnc
    mkOptLin (cont ++ [TypeStr]) vtyp xx0 t0 IdentDefault

-- {--- --- optimize op defs before optimizing lin rules - or not ?

  IdentOper ltyp trm -> do
    (_,vtyp) <- ltypeForm ltyp
    (xx,t) <- return $ scanAbs trm

    let t' = case optim st ident vtyp xx t of
               Ok t' -> t'
               _ -> t       -- since case exp fails with type variables. 23/1/2001
    return (ident,IdentOper ltyp (mkAbs xx t'))  

---}

  IdentLType ty -> do
    ty' <- computeLType (snd st) ty
    return (ident,IdentLintype ty')
  IdentLintype ty -> do
    ty' <- computeLType (snd st) ty
    return (ident,IdentLintype ty')
  _ -> return ii ---
 where 
   scanAbs (Abs x b) = (x:b1, b2) where (b1,b2) = scanAbs b
   scanAbs t = ([],t)

   mkOptLin cont vtyp xx0 t0 inf = do
    let lco    = length cont
        lc     = lco - length xx0
        xx1    = if lc < 1 then [] else mkIdents xx0 lc

        Ident (_,(df,0)) = ident  -- these two are awful. AR 18/5/2001
        xx2    = [Ident (x,(df,i)) | (Ident (x,_),i) <- zip xx1 [lco+1..]]

        (xx,t) = (xx0 ++ xx2, mkApp t0 (map Var xx2))
    t' <- optim st ident vtyp xx t
    return (ident,inf xx t')  


optim st@(abs,cnc) ident@(Ident (_,(int,_))) vtyp xx t = do
    let ren = renameTerm int st Nothing xx
    t2 <- defexpand (flip lookupLin cnc) t ---- (lookupLinAbs cnc) t
    t3 <- return $ fst $ ren (t2,0) -- to avoid captures
    t4 <- betaconv t3 --- betaOnlyConv would leave Lets alone

----    t8 <- partEval cnc xx vtyp t4 ---- work on this to replace the following

----
    t5 <- caseExpand st ident vtyp t4 --- doesn't select for unknown tag !
    t6 <- return $ fst $ ren (t5,0)
    t8 <- tryOptCompute t6 ----
----

    normalizeToks t8


tryOptCompute trm = case trm of
  Select (Cases [([PattVar x],val)]) [t] -> do
    subst <- return [(x,t)]  --- for multiple patts as well - or curry them !!
    trm' <- substitute subst val
    tryOptCompute trm'
  Select (Cases cases) ts | all isInConstantForm ts -> do
    ts' <- mapM tryOptCompute ts         -- don't select for unknown tag !
    cases' <- mapPairListM (\ (_,c) -> tryOptCompute c) cases
    (val,subst) <- findMatch cases' ts'
    trm' <- substitute subst val
    tryOptCompute trm'

---  Select (Select t@(Cases _) x) y -> do -- this does not work. 1/12/2000
---    t' <- tryOptCompute (Select t y)
---    tryOptCompute (Select t' x)

  Select (Select (Cases cc) x) y -> do
    cc' <- mapPairListM (\ (_,t) -> tryOptCompute (Select t y)) cc
    tryOptCompute (Select (Cases cc') x)

{-  these do not help; composOp should do this job.
  Cases cc -> do
    cc' <- mapPairListM (\ (_,t) -> tryOptCompute t) cc
    return $ Cases cc'
  Record cc -> do
    cc' <- mapPairListM (\ (_,t) -> tryOptCompute t) cc
    return $ Record cc'
-}

  Project (Record r) i -> do
    t <- lookupErr i r
    tryOptCompute t

  _ -> composOp tryOptCompute trm


----------------
--- simplify all this !

{-
partEval :: Token a => ConcreteST a -> [Ident] -> LType a -> LTerm a -> Err (LTerm a)
partEval st xx ty tr = case ty of
    RecType rr -> embedM Record $ expRec tr rr
    Table cc v -> embedM Cases  $ expTable tr v cc
    _ -> case tr of
      Project (Var x) l | elem x xx -> return tr
      Select  (Var x) l | elem x xx -> return tr
      _ -> composOp (pe dummyType) tr
  where
    pe = partEval st xx
    dummyType = TypeType --- where the type does not matter
    expRec t = mapPairListM (\ (l,f) -> pe f (Project t l))
    expTable t v cc = do
      pp <- lookupTagss cc st
      tt <- mapM  (pe v . Select t) pp
      xx <- mapM (mapM term2patt) pp
      return $ zip xx tt

embedM c m = do
  x <- m
  return $ c x
-}
------------



lookupLinAbs :: Token a => ConcreteST a -> Ident -> Err (LTerm a)
lookupLinAbs cnc op = case lookupLin op cnc of
  Ok (t@(Abs _ _)) -> return t --- and Let (Abs _ _) !
  Ok _ -> return (Cons op)
  _ -> prtBad "def expansion fails in optimization for constant" op

recordExpand dd t = Record [(l, Project t l) | (l,_) <- dd]

caseExpand :: Token a => GrammarST a -> Ident -> LType a -> LTerm a -> Err (LTerm a)
caseExpand st@(abs,cnc) fun vtyp trm = do 
  (case (vtyp,trm) of
     (RecType dd, Var x) -> do
        let trm' = recordExpand dd trm -- to create CF by mere substitution
        caseExpand st fun vtyp trm'
     (RecType dd, Record rr) -> do
        let ll  = map fst dd
            rr' = [(l,v) | (l,v) <- rr, elem l ll] -- removes superfluous fields
        rr'' <- mapM (expandTableField cnc dd) rr'
        return (Record rr'')
     _ -> expandInTerm cnc vtyp trm)

expandTableField :: Token a => 
  ConcreteST a -> [Labelling a] -> Assign a -> Err (Assign a)
expandTableField st labls (lab,term) = do
  typ <- lookupErrMsg 
             ("expanding" +++ prt term +++ "in" +++ prt (RecType labls)) lab labls
  term' <- expandInTerm st typ term
  return (lab,term')

expandInTerm :: Token a => ConcreteST a -> LType a -> LTerm a -> Err (LTerm a)
expandInTerm st typ trm0 = case typ of
  Table tt val -> do
     trm <- caseselectEtc trm0
     cc1 <- expandCases tt trm st
     cc2 <- mapPairListM (\ (_,t) -> caseselectEtc t) cc1
     cc3 <- mapPairListM (\ (_,t) -> expandInTerm st val t) cc2
     return (Cases cc3) 
  _ -> return trm0 --- N.B. assumes tables are uncurried
 where caseselectEtc t = caseselect t --- >>= betaconv >>= normalizeToks

normalizeToks :: Token a => LTerm a -> Err (LTerm a)
normalizeToks t =
 case t of
   Tok t | isZeroTok t -> return $ Tok zeroTok
   Concat (Tok t) s2 | isZeroTok t -> normalizeToks s2 
   Concat s1 (Tok t) | isZeroTok t -> normalizeToks s1 -- reading from left to right
---   Concat (Alts DPrefix (t,tt)) (Tok s1) -> 
   Glue a@(Alts DPrefix ttt) t2 -> do
     t2' <- normalizeToks t2
     ttt' <- lterm2alts a
     return $ case t2' of
                Tok s2 -> Tok (joinByFix (flip glueTok) isPrefixTok ttt' s2)
		_      -> Glue a t2'
   Glue t1 t2 -> do
     t1' <- normalizeToks t1
     t2' <- normalizeToks t2
     return $ case (t1',t2') of
                (Tok s1, Tok s2) -> Tok (glueTok s1 s2)
		_      -> Glue t1' t2'
   Alts d (t,tt) -> do
     t' <- normalizeToks t
     tt' <- mapM normConds tt
     return (Alts d (t', concat tt'))
   _ -> composOp normalizeToks t
   --- more could be done ...
  where
   normConds (Strs conds,var) = do
     var' <- normalizeToks var
     conds' <- mapM normalizeToks conds
     return [(Strs [cond'], var') | cond' <- conds] 
   normConds vc = return [vc]


