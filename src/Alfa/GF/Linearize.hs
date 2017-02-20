module Linearize where

import Operations
import Tokens
import Grammar
import Macros
import SymbolTable
import ComputeTerm
import Lookup
import Predefined
import PrGrammar
import LinTypes
import Rename

-- AR 4/12/1999 -- 5/6/2000

-- to get lin's for all combinations of tags ; 
-- this assumes the lin rules of the grammar have been optimized by case expansion

allLinearizes :: Token a => Trm -> GrammarST a -> Err [a]
allLinearizes t g = do
  ll <- allLinearizes' t g
  return $ map snd ll

allLinearizes' :: Token a => Trm -> GrammarST a -> Err [([String],a)]
allLinearizes' trm gr = do 
  trm' <- linTerm trm gr  
  strm <- allLinCombinations trm' gr
  tt <- mapM (lterm2toks . snd) strm
  let pp = map (map (prTList "," . map prt) . fst) strm
  return $ zip pp tt

allLinCombinations :: Token a => Term a -> GrammarST a -> Err [([[Patt]],Term a)]
allLinCombinations trm gr@(_,conc) = case trm of
  Record rr -> case lookup (linLabel 0) rr of
    Just t -> return $ allCases t
    _ -> prtBad "no Str field in" trm
  _ -> prtBad "expected record as linearization instead of" trm
 where
  allCases t = case t of
    Cases cc -> [(p:pp,t'') | (p,t') <- cc, (pp,t'') <- allCases t']
    t -> [([],t)]

-- to get the first linearization
firstLinearize :: Token a => Trm -> GrammarST a -> Err a
firstLinearize t gr =
 case allLinearizes t gr of
   Ok (s:_) -> return s
   Ok _     -> prtBad "empty list of linearizations for" t
   Bad msg  -> prtBad (msg +++ "thus no lin of") t

-- this is the compositional linearization function
--- linTerm :: Token a => Trm -> GrammarST a -> Err (LTerm a)
linTerm trm st@(_,gr) = 
 do (binds,atom,terms) <- termForm trm
    let vars = [Tok (readTok (prt v)) | v <- binds]
    (fun,xx) <- linAtom atom st
    if length xx > length terms then prtBad "too few arguments for" atom else
      do args <- errIn ("linearizing" +++ prt trm) (mapM (flip linTerm st) terms)
         body <- linCompute gr (zip xx args) fun
         plusRecord (mkRecord varLabel vars) body

-- linearize function, variable, or metavariable
--- linAtom :: Token a => Trm -> GrammarST a -> Err (LTerm a, [Ident])
linAtom atom st@(_,gr) = case atom of
  Cons fun -> case lookupConcrete fun gr of
    Ok (IdentLin xx t) -> return (t,xx)
    _ -> linDefaultOfFun fun st
  Typed var typ -> do
    cat  <- valCat typ
    typ0 <- lookupLinTypeOfCat cat gr
    typ  <- computeLType gr typ0
    t    <- linDefaultOfLType var typ gr
    return (t,[]) --- 5/6 simplifying variable syntax
  Literal _ s -> return (linearizeLiteral s,[])
  ArgVar (c,i) -> return (ArgVar (c,i),[])
  Meta (MetaSymb (cat,i)) ->
    do typ0 <- lookupLinTypeOfCat cat gr
       typ  <- computeLType gr typ0
       t <- linDefaultOfLType atom typ gr
       return (t,[])
  Closure _ t -> linAtom t st
  _ -> prtBad "cannot linearize atom" atom

{---
linDefault :: Token a => GrammarST a -> Type -> Trm -> Err (LTerm a)
linDefault st@(_,gr) typ trm = do
  (_,cat,tt) <- typeForm typ
  (xx, t)    <- case lookupDefaultOfCat cat gr of
     Ok xt -> return xt
     Bad b -> do 
       t <- linDefaultOfLType trm typ gr
       return ([],t)
  tt'        <- mapM (flip linTerm st) tt
  linCompute gr (zip xx (tt' ++ [Tok (readTok (prt trm))])) t
-}

-- to get a token list as final result ; assuming t computed
lterm2toks :: Token a => LTerm a -> Err a
lterm2toks t = lterm2alts t >>= (return . fst)

lterm2alts t =
  case t of
    Tok a -> return $ noVariants a
    Concat s1 s2 -> 
     do a1 <- lterm2alts s1
        a2 <- lterm2alts s2
        return (plusPrefixAlts a1 a2)
    Glue s1 s2 -> 
     do a1 <- lterm2alts s1
        a2 <- lterm2alts s2
        return (gluePrefixAlts a1 a2)
    Alts DPrefix aa -> 
     do (a',aa') <- ltAltern aa
        return (a',aa')
    Alts DPostfix aa -> 
     do (a',_) <- ltAltern aa
        return $ noVariants a' ---
    Strs [t] -> lterm2alts t ---
    _ -> prtBad "no token list computable from" t
 where
  ltAltern (a,aa) = do
      a' <- lterm2toks a
      aa' <- mapM (pairM lterm2toks) aa
      return (a',aa')

