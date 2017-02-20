module LinTypes where

import Operations
import Tokens
import Grammar
import Macros
import ComputeTerm
import SymbolTable
import Lookup
import PrGrammar

-- AR 4/12/1999 -- 30/3/2000

--Some type signatures have been commented out to avoid a bug in nhc98-1.00 /TH

-- but if you remove this then hbc gets confused in Linearize ! -- AR 18/12/2000

-- how to make any configuration work in ghc ??? AR 2/2/2001

linType :: (Token a) => LType a -> ConcreteST a -> Err (LType a)
linType typ gr = 
 do (cont,cat) <- linTypeForm typ gr
    let (vars,args) = unzip cont
    val0 <- lookupLinTypeOfCat cat gr
    val  <- computeLType gr val0
    return (mkProd (zip vars args, val, []))

linTypeForm :: (Token a) => LType a -> ConcreteST a -> Err (Context a, Cat)
linTypeForm t gr =
 case t of
   Prod x a b  ->
     do (x', cat) <- linTypeForm b gr
        a'        <- linArgType a gr
        return ((x,a'):x', cat)
   App c a ->
     do (_,cat) <- linTypeForm c gr
        return ([],cat)
   Cons c ->
     return ([],c) 
   Var c ->
     return ([],c) 
   _       -> 
     prtBad "no lin type form of type" t

linArgType :: (Token a) => LType a -> ConcreteST a -> Err (LType a)
linArgType typ gr = 
 do (cont,cat) <- linTypeForm typ gr
---    argcats <- mapM (valCat . snd) cont
---    args <- mapM (flip lookupLinTypeOfCat gr) argcats
    let args = replicate (length cont) TypeStr --- 5/6 simplifying variable syntax
    val0 <- lookupLinTypeOfCat cat gr
    val  <- computeLType gr val0
    plusRecType (mkRecType varLabel args) val

linDefaultOfLType :: (Token a) => Trm -> LType a -> ConcreteST a -> Err (LTerm a)
linDefaultOfLType trm ty gr = lindef [] trm ty where 
 lindef zz tr typ = case typ of
  Prod _ _ _ -> do
    (xx,c) <- ltypeForm typ
    b' <- lindef xx tr c
    return (mkAbs (map fst xx) b') --- refresh idents
  Table xx c -> do
    b' <- lindef zz tr c
    return (Cases [(replicate (length xx) wildPatt, b')])
  RecType r -> do
    let (ll,tt) = unzip r
    tt' <- mapM (lindef zz tr) tt
    return (Record (zip ll tt'))
  TypeStr -> do
    tr' <- case tr of
      ArgVar ci -> return (ArgVar ci) -- return tr gives wrong overloading
      Var x -> return $ Tok (readTok (prt tr))
      _ -> do zz' <- mapM linDefVar zz
              return (foldl Concat (Tok (readTok (prt tr))) zz')
    return $ if null zz then tr' else 
                  (Concat (Tok (readTok "(")) (Concat tr' (Tok (readTok ")"))))
  Cons c -> do
    case tr of
      ArgVar ci -> return (Project (ArgVar ci) (Label (prt c,0))) ---
      _ -> defaultTag typ gr
  _ -> prtBad "cannot find default lin of type" typ
 linDefVar (var,typ) = 
   do params <- lookupParamsOfLType gr typ
      (s1,tags) <- case params of
                     (i,tt):_ -> do
                        tt' <- mapM (flip defaultTag gr) tt
                        return (i,tt')
                     _      -> return (0,[])
      return (mkSelect (Project (Var var) (linLabel s1)) tags) ---

-- defaultTag :: (Token a, Token b) => LType b -> ConcreteST a -> Err (LTerm a)
defaultTag tagtype st = do
  tags <- lookupTags tagtype st
  (case tags of
     t:_ -> return t
     _ -> prtBad "empty tag type" tagtype)

inferValCat :: (Token a) => Trm -> GrammarST a -> Err Cat
inferValCat trm gr@(abs,conc) = 
 do (_,atom,_) <- termForm trm
    (case atom of
       Cons f -> case lookupFun f abs of
         Ok t   -> valCat t
         _      -> prtBad "cannot find val cat for unknown constant" f
       Meta (MetaSymb (cat,_)) -> return cat
       _ -> prtBad "atom expected instead of" atom)

lookupParamsOfCat :: (Token a) => Cat -> ConcreteST a -> Err [(Int,[LType a])]
lookupParamsOfCat cat st = 
  do typ  <- lookupLinTypeOfCat cat st
     typ' <- computeLType st typ 
     lookupParamsOfLType st typ'

lookupParamsOfLType :: (Token a) => ConcreteST a -> LType a -> Err [(Int,[LType a])]
lookupParamsOfLType st typ = case typ of
        RecType r -> 
          do let its = [(i,t) | (l@(Label (_,i)), t) <- r, l == linLabel i]
             pp <- mapM (paramsOfTable . snd) its
             return (zip (map fst its) pp) 
        t -> prtBad "record lin type expected instead of" t

computeLType :: (Token a) => ConcreteST a -> LType a -> Err (LType a)
computeLType st ty = case ty of                    --- a,b is a workaround for ghc
   Cons c -> case lookupLinTypeMacro c st of
     Ok (Cons c') | eqStrIdent c' c -> return ty
     Ok ty' -> complt ty'
     _ -> prtBad "lin type computation fails for constant" c
   LiT ty0 -> do
     ty1 <- linType ty0 st
     complt ty1

   UpdRecType r (l,v) -> do
     r' <- complt r
     v' <- complt v
     updRecType r' (l,v') 

   Let dd b -> do    --- taken from ComputeTerm.betaconv
     let (xx,aa) = unzip [(x,a) | (x,_,a) <- dd]
     aa' <- mapM complt aa
     b'  <- substitutes (zip xx aa') b
     computeLType st b'  
   App (Abs x b) a -> do
     a' <- complt a
     b' <- complt b
     b'' <- substitute [(x,a')] b'
     complt b''
   App b a -> do
     a' <- complt a
     b' <- complt b
     (if a'==a && b'==b then return else complt) $ App b' a'

   _ -> composOp complt ty
 where complt = computeLType st

-- eqLType :: Token a => ConcreteST a -> LType a -> LType a -> Err Bool
eqLType st ty1 ty2 = do
  ty1' <- computeLType st ty1
  ty2' <- computeLType st ty2
  return $ if ty1' `alphaEq` ty2' then True else False

linTypeOfFun :: (Token a) => Fun -> GrammarST a -> Err (LType a)
linTypeOfFun fun st@(abs,cnc) = do
  typ <- lookupFun fun abs
  typ' <- type2ltype typ
  ltyp0 <- linType typ' cnc
  computeLType cnc ltyp0

type2ltype :: (Token a, Token b) => LType a -> Err (LType b)
type2ltype t = case t of
  Prod x a b -> do
    a' <- type2ltype a
    b' <- type2ltype b
    return $ Prod x a' b'
  App f a -> do
    f' <- type2ltype f
    a' <- type2ltype a
    return $ App f' a'
  Abs x b -> do
    b' <- type2ltype b
    return $ Abs x b'
  Var x  -> return $ Var x
  Cons c -> return $ Cons c
  Literal p q -> return $ Literal p q
  TypeType -> return TypeType
  _ -> prtBad "cannot make type2ltype" t


linDefaultOfFun :: Token a => Fun -> GrammarST a -> Err (LTerm a, [Ident])
linDefaultOfFun fun st@(_,cnc) = do
  ltyp <- linTypeOfFun fun st
  ltrm <- linDefaultOfLType (Cons fun) ltyp cnc
  (xx,f,aa) <- termForm ltrm
  return (mkApp f aa, xx) --- mkApp needed because of termForm

-- lin arg type structure {var :: Str, var1:: Str, inh::Inh0, lin :: pars0 -> strs}
-- for X of this type, the vars are X.var,...
-- the functions are X.lin,...
-- the inherent features are X.inh,...
-- see Macros for a nicer way to access these objects

