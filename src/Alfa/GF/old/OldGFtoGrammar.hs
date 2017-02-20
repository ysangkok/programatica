module OldGFtoGrammar where

import Operations
import OldGFParsers((***))
import Tokens
import Grammar
import PrGrammar
import Macros
import qualified OldGF as O
import qualified OldPGF as P

-- AR 5/12/1999 -- 29/3/2000

-- to interpret old GF files in the new GF
--- N.B. the translations onLVars, aritStrOp, dissocStrOp are unsafe :
--- they may produce grammar check errors in the resulting grammar.
--- Making them safe would require heavy lookups or slight changes in
--- the old GF syntax.
 
onGrammar :: O.Grammar -> Grammar [Str]
onGrammar gr@(_,th) = Grammar (onAbstract th, onConcrete gr)

-- theory, terms, and types

onAbstract :: O.Theory -> Abstract
onAbstract th@(cats,vars,rules,defs) = 
 Abstract (onDecls th ++ [onDef d | d <- defs, nocatDef d]) where
   nocatDef d =  -- we ignore the cat defs of old GF, which are obsolete
     case d of
       O.DfData _ _ -> False
       O.DfRec (O.CPatt _ _) _ -> False
       _ -> True 

onDecls :: O.Theory -> [Def]
onDecls th@(cats,vars,rules,defs) = map onCat cats ++ map onFun rules 

onCat :: (O.Ident,O.CatDef) -> Def
onCat (i,(context,_,_,_)) = DefCat (zIdent i) (onContext context)

onFun :: (O.Ident,O.Rule) -> Def
onFun (i,(typ,_)) = DefFun (zIdent i) (onType typ)

onType :: O.Type -> Type
onType t = mkProd (onContext cont, Var (zIdent cat), map onTerm terms) where 
 (cont, O.Cat cat, terms) = O.typeForm t

onTerm :: O.Term -> Trm
onTerm t = mkTerm (map onSymb xx, onAtom tr, map onTerm terms) where 
 (xx, tr, terms) = O.termForm t
 onAtom at =
  case at of
    O.Cons (O.Fun f)   -> Var (zIdent f)
    O.Var (O.Symb x)   -> Var (zIdent x)
    O.Meta (O.Cat c,s) -> Meta (MetaSymb (zIdent c,length s)) --- lose s

onContext :: O.Context -> Cont
onContext cont = [(onSymb x, onType t) | (x, t) <- cont]

onDef :: O.Definition -> Def
onDef (O.DfRec patt term) = DefDef (onPatt patt) (onTerm term) where
 onPatt p =
  case p of  --- O.CPatt is ignored above in onTheory only as outermost form
    O.APatt x -> PattVar (onSymb x)
    O.FPatt (O.Fun f) patts -> PattCons (zIdent f) (map onPatt patts)

onSymb :: O.Symb -> Ident
onSymb (O.Symb x) = zIdent x

-- concrete syntax

onConcrete :: O.Grammar -> Concrete [Str]
onConcrete gr@(lin,th@(cats,vars,rules,_)) = 
  Concrete (onLDefs lin ++ map onLCat cats ++ map (onLFun gr) rules ++ onLVars vars)

onLDefs :: O.Lin -> [LDef [Str]]
onLDefs lin@(tok,pars,ops) = onTok tok ++ map onPar pars ++ map onOper ops

onTok (O.Tokenizer ident _) = [DefTokenizer (zIdent ident)]

onLCat :: (O.Ident,O.CatDef) -> LDef [Str]
onLCat (cat,(_,pars,inhs,ar)) = 
 DefLintype (zIdent cat) (RecType (inhs' ++ str')) where
  pars' = [Var (zIdent t) | (n, O.TagType t) <- zip [0..] pars]
  RecType inhs' = mkRecType inhLabel [Var (zIdent t) | O.TagType t <- inhs]
  RecType str' = case ar of
    1 -> mkRecType    linLabel               [mkTable pars' TypeStr]
    _ -> mkRecTypeN 1 linLabel (replicate ar (mkTable pars' TypeStr))

onPar :: (O.Ident,[(O.Tag,[O.TagType])]) -> LDef [Str]
onPar (i,tt) = DefParam (zIdent i) [(zIdent t,onTT co) | (O.Tag t, co) <- tt] where
 onTT co = mkContext [Var (zIdent ty) | O.TagType ty <- co]

onOper :: (O.Ident,O.OpDef) -> LDef [Str]
onOper (i,def) = DefOper (zIdent i) ty def' where (ty,def') = onOpDef [] def

onLVars :: [O.VarSpec] -> [LDef [Str]]
onLVars vars = concat (map onLVar vars) where
  onLVar (ids,cats) = 
    [DefVar (zIdent x) (map (\ (O.Cat c) -> zIdent c) cats) | x <- ids]

onLFun :: O.Grammar -> (O.Ident,O.Rule) -> LDef [Str]
onLFun gr (i,rule@(typ,_)) = DefLin (zIdent i) args (onRule gr args rule) where
 args = case typeForm (onType typ) of 
          Ok (cont,_,_) -> map fst cont
          _ -> [] --- should not happen ?

onRule :: O.Grammar -> [Ident] -> O.Rule -> LTerm [Str]
onRule gr args rule@(typ,entry@(strop,feats)) = Record (inhs ++ lins) where
 Record inhs = mkRecord inhLabel (map (onFeature args) feats)
 Record lins = mkRecord linLabel (dissocStrOp (onStrOp args strop))

type BEnv = [Ident] -- the variables bound to arguments

onOpDef :: BEnv -> O.OpDef -> (LType [Str], LTerm [Str])
onOpDef bb def =
 case def of
   O.DefStrOp tys strop -> 
     (mkTable [Var (zIdent t) | O.TagType t <- tys] 
             (case (aritStrOp strop) of
                1 -> TypeStr
                i -> mkRecTypeN 1 strLabel (replicate i TypeStr)),  
      onStrOp bb strop)
   O.DefFeatOp tys (O.TagType t) fop -> 
     (mkTable [Var (zIdent t) | O.TagType t <- tys] (Var (zIdent t)), 
      onFeatOp bb fop)
   O.DefNewOp cont tys strop ->
     let cont' = [(zIdent x, onNewLinType lty) | (x,lty) <- cont] in 
      (mkProd
        (cont',
         mkTable [Var (zIdent t) | O.TagType t <- tys] 
             (case (aritStrOp strop) of
                1 -> TypeStr
                i -> mkRecTypeN 1 strLabel (replicate i TypeStr)),
         []),  
         mkAbs (map fst cont') (onStrOp bb strop))

onNewLinType :: O.NewLinType -> LType [Str]
onNewLinType (tagtypes,int) = 
  mkTable [Var (zIdent t) | O.TagType t <- tagtypes] 
          (case int of
                1 -> TypeStr
                i -> mkRecTypeN 1 strLabel (replicate i TypeStr))

onStrOp :: BEnv -> O.StrOp -> LTerm [Str]
onStrOp bb op =
 case op of
   O.StrOpId i -> Var (zIdent i)
   O.StrOpCase [([],ss)] -> onStrs bb ss
   O.StrOpCase cc -> Cases [(map onTagPatt tt, onStrs bb ss) | (tt,ss) <- cc]

aritStrOp :: O.StrOp -> Int
aritStrOp op = 
 case op of
   O.StrOpCase ((_,ss):_) -> length ss
   _ -> 1 ---
--- grammar check error if the value type of op has arity > 1

dissocStrOp :: LTerm [Str] -> [LTerm [Str]]
dissocStrOp op = case op of
  Cases cc@((_,Record r):_) -> [Cases [(p, Project t l) | (p,t) <- cc] | (l,_) <- r]
  _ -> [op]
--- grammar check error if arity > 1 but the first case is not an explicit record

onStrs :: BEnv -> [O.Str] -> LTerm [Str]
onStrs bb ss =
 case ss of
   [s] -> onStr bb s
   _   -> mkRecord linLabel (map (onStr bb) ss)

onStr :: BEnv -> O.Str -> LTerm [Str]
onStr bb s = 
 case s of
   O.StrCons ss [] -> Tok (ms ss)
   O.StrCons ss aa -> 
     Alts DPrefix 
       (Tok (ms ss),[(Strs [Tok [Str c] | c <- vv],Tok (ms cc)) | (vv,cc) <- aa])
   O.StrComb s1 True s2 -> Glue (onStr bb s1) (onStr bb s2)
   O.StrComb s1 _ s2 -> Concat (onStr bb s1) (onStr bb s2)
   O.StrArg x i f -> mkSelect (linOfArg i c) (map (onFeature bb) f) where c = bb !!x
   O.StrBound x i -> varOfArg i (bb !! x)
   O.StrApp op s f 0 -> 
     mkSelect (mkApp (onStrOp bb op) (map (onStr bb) s)) (map (onFeature bb) f)
   O.StrApp op s f i ->
     Project (mkSelect (mkApp (onStrOp bb op) (map (onStr bb) s)) 
                                               (map (onFeature bb) f)) (strLabel i)
   O.StrLet i opd str -> 
     Let [(zIdent i,ty,t)] (onStr bb str) where (ty,t) = onOpDef bb opd
   O.StrNewArg ident xx -> mkSelect (Var (zIdent ident)) (map (onFeature bb) xx)
 where ms = map Str

onFeature :: BEnv -> O.Feature -> LTerm [Str]
onFeature bb f =
 case f of
   O.FeatAtom te -> onTagExp te
   O.FeatArg x i -> inhOfArg i (bb !! x)
   O.FeatApp fop feats -> mkSelect (onFeatOp bb fop) (map (onFeature bb) feats)

onFeatOp :: BEnv -> O.FeatOp -> LTerm [Str]
onFeatOp bb op =
 case op of
   O.FeatOpId i -> Var (zIdent i)
   O.FeatOpCase cc -> Cases [(map onTagPatt tt, onFeature bb f) | (tt,f) <- cc]

onTagExp :: O.TagExp -> LTerm [Str]
onTagExp te =
 case te of
   O.TagCons (O.Tag t) -> Var (zIdent t)
   O.TagVar x ->  Var (zIdent x)
   O.NewTagExp (O.Tag t) xx -> mkApp (Var (zIdent t)) (map onTagExp xx)

onTagPatt :: O.TagExp -> Patt
onTagPatt te =
 case te of
   O.TagCons (O.Tag t) -> PattCons (zIdent t) []
   O.TagVar x -> PattVar (zIdent x)
   O.NewTagExp (O.Tag t) xx -> PattCons (zIdent t) (map onTagPatt xx)

-- printing and parsing abstract syntax in old GF style

prOldTrm trm = case trm of
  Meta (MetaSymb (c,i)) -> "[" ++ prt c ++ show i ++ "]"
  Prod x a b -> "(" ++ prt x ++ ":" ++ prOldTrm a ++ ")" ++ prOldTrm b
  Abs x b -> prParenth (prt x) ++ prOldTrm b
  App _ _ -> case termForm trm of
    Ok (_,c,aa) -> prOldTrm c ++ prArgList (map prOldTrm aa)
  _ -> prt trm

pOldTrm :: String -> [Trm]
--pOldTrm s = [(onTerm t,s') | (t,s') <- P.pTerm P.emptyGPEnv s]
pOldTrm = map fst . (P.pTerm P.emptyGPEnv *** onTerm)
