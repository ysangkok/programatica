module Macros where

import Operations
import Tokens
import Grammar
import PrGrammar
import List (sortBy,nub)

-- AR 7/12/1999 - 9/5/2000

-- operations on terms and types not involving lookup in or reference to grammars

typeForm :: Type -> Err (Cont, Cat, [Trm])
typeForm t =
 case t of
   Prod x a b  ->
     do (x', cat, args) <- typeForm b 
        return ((x,a):x', cat, args)
   App c a ->
     do (_,cat, args) <- typeForm c
        return ([],cat,args ++ [a])
   Cons c ->
     return ([],c,[]) 
   Var c ->
     return ([],c,[]) 
   _       -> 
     prtBad "no normal form of type" t

valCat :: Type -> Err Cat
valCat typ = 
  do (_,cat,_) <- typeForm typ
     return cat

valType :: Type -> Err Type
valType typ = 
  do (_,cat,xx) <- typeForm typ --- not optimal to do in this way
     return $ mkApp (Cons cat) xx

typeSkeleton :: Type -> Err ([(Int,Cat)],Cat)
typeSkeleton typ =
  do (cont,cat,_) <- typeForm typ
     args <- mapM (typeForm . snd) cont
     return ([(length c, v) | (c,v,_) <- args], cat)

catSkeleton :: Type -> Err ([Cat],Cat)
catSkeleton typ =
  do (args,val) <- typeSkeleton typ
     return (map snd args, val)

isRecursiveType :: Type -> Bool
isRecursiveType t = errVal False $ do
  (cc,c) <- catSkeleton t -- thus recursivity on Cat level
  return $ any (eqCat c) cc

contextOfLType :: Type -> Err Cont
contextOfLType typ = 
  do (cont,_) <- ltypeForm typ
     return cont

ltypeForm :: LType a -> Err (Context a, LType a)
ltypeForm t =
 case t of
   Prod x a b  ->
     do (x', typ) <- ltypeForm b 
        return ((x,a):x', typ)
   _ ->
     return ([],t) 

termForm :: Term a -> Err ([(Symb)], Term a, [Term a])
termForm t =
 case t of
   Abs x b  ->
     do (x', fun, args) <- termForm b 
        return (x:x', fun, args)
   App c a ->
     do (_,fun, args) <- termForm c
        return ([],fun,args ++ [a]) 
   _       -> 
     return ([],t,[])

varsOfType :: Type -> [Ident]
varsOfType t = case t of
  Prod x _ b -> x : varsOfType b
  _ -> []

mkProd :: (Context a, Term a, [Term a]) -> Term a
mkProd ([],typ,args) = mkApp typ args
mkProd ((x,a):dd, typ, args) = Prod x a (mkProd (dd, typ, args))

mkTerm :: ([(Symb)], Term a, [Term a]) -> Term a
mkTerm (xx,t,aa) = mkAbs xx (mkApp t aa)

mkApp :: Term a -> [Term a] -> Term a
mkApp t = foldl App t

appc :: String -> [Trm] -> Trm
appc s tt = mkApp (Cons (zIdent s)) tt

mkTable :: [Term a] -> Term a -> Term a
mkTable [] t = t
mkTable tt t = Table tt t

mkSelects :: Term a -> [Term a] -> Term a
mkSelects t [] = t
mkSelects t (x:xs) = mkSelects (Select t [x]) xs

mkSelect :: Term a -> [Term a] -> Term a
mkSelect t [] = t
mkSelect t tt = Select t tt

mkAbs :: [Ident] -> Term a -> Term a
mkAbs xx t = foldr Abs t xx

paramsOfTable :: Token a => LTerm a -> Err [LTerm a]
paramsOfTable (Table xx _) = Ok xx
paramsOfTable _ = Ok []
--- paramsOfTable t = prtBad "to get params of table need a table instead of" t

valOfTable :: Token a => LTerm a -> Err (LTerm a)
valOfTable (Table _ t) = Ok t
valOfTable t = prtBad "to get value type of table need a table instead of" t

zIdent :: String -> Ident
zIdent s = Ident (s,(0,0))

strangeIdent s = Ident ("BOOO"++ s,(666, 0))

bareIdent :: Ident -> Ident
bareIdent = zIdent . symbolOfIdent

mkIdent :: String -> Int -> Ident
mkIdent s i = zIdent (s ++ show i)

lookupVar :: Ident -> [(Ident,a)] -> Maybe a
lookupVar x g = lookup (bareIdent x) [(bareIdent z,y) | (z,y) <- g]

symbolOfIdent :: Ident -> String
symbolOfIdent (Ident (x,_)) = x

symid = symbolOfIdent

catOfMeta (MetaSymb (c,_)) = c

mkRecTypeN :: Int -> (Int -> Label) -> [LType a] -> LType a
mkRecTypeN int lab typs = RecType [ (lab i, t) | (i,t) <- zip [int..] typs]

mkRecType :: (Int -> Label) -> [LType a] -> LType a
mkRecType = mkRecTypeN 0

plusRecType :: Token a => LType a -> LType a -> Err (LType a)
plusRecType t1 t2 =
 case (t1,t2) of
   (RecType r1, RecType r2) -> return (RecType (r1 ++ r2))
   _ -> Bad ("cannot form record type" +++ prt t1 +++ "+" +++ prt t2)

updRecType :: Token a => LType a -> Labelling a -> Err (LType a)
updRecType t1 lv =
 case t1 of
   RecType r1 -> return (RecType (lv : r1)) -- lookup from head
   _ -> return $ UpdRecType t1 lv --- ("cannot update record type" +++ prt t1)

updRecord :: Token a => LTerm a -> Assign a -> Err (LTerm a)
updRecord t1 lv =
 case t1 of
   Record r1 -> return (Record (lv : r1)) -- lookup from head
   _ -> return $ UpdRecord t1 lv --- Bad ("cannot update record" +++ prt t1)

defaultLinType :: LType a
defaultLinType = mkRecType linLabel [TypeStr]

mkRecordN :: Int -> (Int -> Label) -> [LTerm a] -> LTerm a
mkRecordN int lab typs = Record [ (lab i, t) | (i,t) <- zip [int..] typs]

mkRecord :: (Int -> Label) -> [LTerm a] -> LTerm a
mkRecord = mkRecordN 0

plusRecord :: Token a => LTerm a -> LTerm a -> Err (LTerm a)
plusRecord t1 t2 =
 case (t1,t2) of
   (Record r1, Record r2) -> return (Record (r1 ++ r2))
   _ -> Bad ("cannot form record" +++ prt t1 +++ "+" +++ prt t2)

wildPatt :: Patt
wildPatt = PattVar (zIdent "_")

isWildIdent :: Ident -> Bool
isWildIdent = eqStrIdent (zIdent "_")

mkDecl :: Term a -> Decl a
mkDecl typ = (zIdent "_", typ)

-- pattern matching

term2patt :: Token a => Term a -> Err Patt
term2patt trm = case termForm trm of
  Ok ([], Var x, []) -> return (PattVar x)
  Ok ([], Cons c, aa) -> do
    aa' <- mapM term2patt aa
    return (PattCons c aa')
  Ok ([], Record r, []) -> do
    let (ll,aa) = unzip r
    aa' <- mapM term2patt aa
    return (PattRec (zip ll aa'))
  _ -> prtBad "no pattern corresponds to term" trm

patt2term :: Patt -> Term a
patt2term (PattVar x) = Var x
patt2term (PattCons c pp) = mkApp (Cons c) (map patt2term pp)
patt2term (PattRec r) = Record [(l,patt2term p) | (l,p) <- r] 

patt2termMeta :: Patt -> Term a
patt2termMeta p = case p of
  PattVar x 
    | beg == "_"  -> Meta (MetaSymb (zIdent beg,length rest))
    | otherwise   -> patt2term p 
   where (beg,rest) = splitAt 1 (symid x)
  _ -> patt2term p

-- to access the parts of constituents of a syntax tree ; cf. LinTypes

varOfArg :: Int -> Ident -> LTerm a
varOfArg i t = Project (Var t) (varLabel i)

linOfArg :: Int -> Ident -> LTerm a
linOfArg i t = Project (Var t) (linLabel i)

inhOfArg :: Int -> Ident -> LTerm a
inhOfArg i t = Project (Var t) (inhLabel i)

findLinLabels :: Token a => LTerm a -> Err [(Int,LTerm a)]
findLinLabels t = case t of
  Record r -> return $ sortBy (\ x y -> compare (fst x) (fst y)) 
                         [(i,t) | (l@(Label (_,i)), t) <- r, l == linLabel i]
  t -> prtBad "lin elements can only be sought in a record, not in" t
mkType :: [Type] -> Type -> Type
mkType tt t = mkProd (mkContext tt, t, [])

mkContext :: [Term a] -> Context a -- x0:A0, x1:A1
mkContext tt = [(mkIdent "x" k, t) | (t,k) <- zip tt [0..]]

varsOfContext :: Context a -> [Ident]
varsOfContext = map fst

analyseType :: Type -> (Cont,Type)
analyseType t = case t of
 Prod x a b -> ((x,a):b1, b2) where (b1,b2) = analyseType b
 _ -> ([],t)

refreshMeta :: [MetaSymb] -> MetaSymb -> MetaSymb
refreshMeta ms (MetaSymb (cat,_)) = mkFreshMeta ms cat

mkFreshMeta :: [MetaSymb] -> Cat -> MetaSymb -- first fresh metavariable for cat
mkFreshMeta ms cat = mkM 0 where
 mkM k = if elem (MetaSymb (cat,k)) ms then mkM (k+1) else MetaSymb (cat,k)

mkFreshMetas :: [MetaSymb] -> [Cat] -> [MetaSymb]
mkFreshMetas ms [] = []
mkFreshMetas ms (x:xs) = let x' = mkFreshMeta ms x in x':mkFreshMetas (x':ms) xs

refreshMetasInTrm :: [MetaSymb] -> Trm -> Trm
refreshMetasInTrm metas = fst . rms metas where
  rms ms trm = case trm of
    Meta m  -> let m' = refreshMeta ms m in (Meta m', [m'])
    App f a -> let (f',msf) = rms ms f 
                   (a',msa) = rms (msf ++ ms) a
               in (App f' a', msf ++ msa)
    Prod x a b -> 
               let (a',msa) = rms ms a 
                   (b',msb) = rms (msa ++ ms) b
               in (Prod x a' b', msa ++ msb)
    Abs x b -> let (b',msb) = rms ms b in (Abs x b', msb)
    _       -> (trm,[])

refreshPatt :: Patt -> Patt
refreshPatt = fst . rp [zIdent "_"] where
  rp xs p = case p of
    PattCons f ps -> (PattCons f ps', xs' ++ xs) where (ps',xs') = rps xs ps
    PattVar x     -> (PattVar x',     x'   : xs) where x' = mkFreshIdent xs x
  rps xs ps = case ps of
    p:pp -> 
      let (p', xs')  = rp xs p
          (pp',xxs') = rps xs' pp
      in (p':pp', xxs')
    _ -> ([],xs)

mkFreshIdent :: [Ident] -> Ident -> Ident -- next fresh variable xi
mkFreshIdent ids i@(Ident (x,k)) = 
 let x' = Ident (x ++ "\39", k) in if zElem i ids then mkFreshIdent ids x' else i  
  where zElem i ids = elem (symid i) (map symid ids) ---

mkFreshIdents :: [Ident] -> [Ident] -> [Ident] 
mkFreshIdents ids [] = []
mkFreshIdents ids (x:xs) = 
  let x' = mkFreshIdent ids x in x':mkFreshIdents (x':ids) xs

mkIdents :: [Ident] -> Int -> [Ident] 
mkIdents ids = mkFreshIdents ids . flip replicate (zIdent "x")

mkNextIdent :: [(Ident,a)] -> Ident 
mkNextIdent cont = head $ mkIdents (map fst cont) 1

eqStrIdent :: Ident -> Ident -> Bool
eqStrIdent (Ident (s,_)) (Ident (s',_)) = s == s'

eqCat :: Cat -> Cat -> Bool
eqCat = eqStrIdent

basicVarSymb :: Ident -> Ident
basicVarSymb (Ident (x0,n)) = let x = takeWhile (/= '\'') x0 in Ident(x,n)

linTypeStr :: LType a
linTypeStr = mkRecType linLabel [TypeStr] -- default lintype {lin :: Str}

linTypeStrs :: Int -> LType a
linTypeStrs 0 = linTypeStr
linTypeStrs i = mkRecTypeN 1 linLabel (replicate i TypeStr) 
-- default lintype {lin1 :: Str ; ... ; lini :: Str}

linAsStr :: a -> LTerm a
linAsStr s = mkRecord linLabel [Tok s] -- default linearization {lin = s}

getLins :: Token a => LTerm a -> Err [(Int, [([Patt],LTerm a)])]
getLins (Record r) = 
 do let ll = [(i, t) | (lab@(Label (_,i)),t) <- r, lab == linLabel i]
    let (ii,tt) = unzip ll
    cc <- mapM getCases tt
    return (zip ii cc) 
getLins t = prtBad "no linearization term" t

getCases :: Token a => LTerm a -> Err [([Patt],LTerm a)]
getCases (Cases cc) = Ok cc
getCases t = prtBad "no case expression" t

mkCases :: [Ident] -> LTerm a -> LTerm a
mkCases [] t = t
mkCases xx t = Cases [(map PattVar xx, t)]

-- to define compositional term functions

composSafeOp :: (LTerm a -> LTerm a) -> LTerm a -> LTerm a
composSafeOp op trm = case composOp (mkMonadic op) trm of
  Ok t -> t
  _ -> error "the operation is safe isn't it ?"
 where
 mkMonadic f = return . f

composOp :: Monad m => (LTerm a -> m (LTerm a)) -> LTerm a -> m (LTerm a)
composOp co trm = 
 case trm of
   App c a -> 
     do c' <- co c
        a' <- co a
        return (App c' a')
   Abs x b ->
     do b' <- co b
        return (Abs x b')
   Prod x a b -> 
     do a' <- co a
        b' <- co b
        return (Prod x a' b')
   Typed c a -> 
     do c' <- co c
        a' <- co a
        return (Typed c' a')
   Select c a -> 
     do c' <- co c
        a' <- mapM co a
        return (Select c' a')
   Table a c -> 
     do a' <- mapM co a
        c' <- co c
        return (Table a' c')
   Record r -> 
     do r' <- mapPairListM (co . snd) r
        return (Record r')
   RecType r -> 
     do r' <- mapPairListM (co . snd) r
        return (RecType r')
   Project t i ->
     do t' <- co t
        return (Project t' i)
   Cases cc -> 
     do cc' <- mapPairListM (co . snd) cc
        return (Cases cc')
   Closure g t -> 
     do g' <- mapPairListM (co . snd) g
        t' <- co t
        return (Closure g' t')
   Let dd b -> 
     do let (xx,aa,tt) = unzip3 dd
        let aa' = aa
---        aa' <-  mapM co aa --- not class valid
        tt' <- mapM co tt
        b'  <- co b
        return (Let (zip3 xx aa' tt') b')
   LiT a -> 
     do a' <- co a
        return (LiT a')
   Concat s1 s2 -> 
     do v1 <- co s1 
        v2 <- co s2
        return (Concat v1 v2)
   Glue s1 s2 -> 
     do v1 <- co s1
        v2 <- co s2
        return (Glue v1 v2)
   Alts d (t,aa) ->
     do t' <- co t
        aa' <- mapM (pairM co) aa
        return (Alts d (t',aa'))
   Strs tt -> 
     do tt' <- mapM co tt
        return (Strs tt')
   _ -> return trm -- covers Tok, Var, Cons, constant types

updateEnv :: (a,[b]) -> [b] -> (a,[b]) 
updateEnv (a,bs) bs' = (a, bs' ++ bs)

mkPlusTok :: Token a => a -> a -> a
mkPlusTok x y 
 | isZeroTok x = y
 | isZeroTok y = x
 | otherwise   = plusTok x y

unionGrammars :: [Grammar a] -> Grammar a
unionGrammars gg = Grammar (Abstract (concat abss), Concrete (concat cncs)) where
 (abss,cncs) = unzip [(abs,cnc) | Grammar (Abstract abs, Concrete cnc) <- gg]

isEmptyGrammar (Grammar (Abstract a, Concrete c)) = null a && null c

tuple2record :: [a] -> [(Label,a)]
tuple2record ts = [(Label ("p",i), t) | (i,t) <- zip [1..] ts]
