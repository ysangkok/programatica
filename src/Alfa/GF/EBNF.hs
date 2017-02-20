module EBNF where

import Operations
import Parsers
import CF
import Tokens
import Grammar
import CFtoGrammar

import List (nub, partition)

-- AR 18/4/2000

-- Extended BNF grammar with token type a
-- put a = String for simple applications

type EBNF a = [ERule a]
type ERule a = (ECat, ERHS a)
type ECat = (String,[Int])

data ERHS a =
   ETerm a
 | ENonTerm (ECat)
 | ESeq (ERHS a) (ERHS a)
 | EAlt (ERHS a) (ERHS a)
 | EStar (ERHS a)
 | EPlus (ERHS a)
 | EOpt (ERHS a)
 | EEmpty

type CFRHS a = [CFItem a String]
type CFJustRule a = (CFCat String, CFRHS a)

ebnf2gf :: Eq a => (a -> b) -> EBNF a -> Grammar [b]
ebnf2gf f ebnf = cf2grammar f (ebnf2cf ebnf, emptyCFPredef)

ebnf2cf :: Eq a => EBNF a -> [CFRule a String String]
ebnf2cf ebnf = [(mkCFFun i rule,rule) | (i,rule) <- zip [0..] (normEBNF ebnf)] where
  mkCFFun i (c, _) = ("Mk" ++ c ++ "_" ++ show i)

normEBNF :: Eq a => EBNF a -> [CFJustRule a]
normEBNF erules = let
  erules1 = [normERule ([i],r) | (i,r) <- zip [0..] erules]
  erules2 = erules1 ---refreshECats erules1 --- this seems to be just bad !
  erules3 = concat (map pickERules erules2)
  erules4 = nubERules erules3
 in [(mkCFCat cat, map eitem2cfitem its) | (cat,itss) <- erules3, its <- itss]

refreshECats :: [NormERule a] -> [NormERule a]
refreshECats rules = [recas [i] rule | (i,rule) <- zip [0..] rules] where
 recas ii (cat,its) = (updECat ii cat, [recss ii 0 s | s <- its])
 recss ii n [] = []
 recss ii n (s:ss) = recit (ii ++ [n]) s : recss ii (n+1) ss
 recit ii it = case it of
   EINonTerm cat  -> EINonTerm (updECat ii cat)
   EIStar (cat,t) -> EIStar (updECat ii cat, [recss ii 0 s | s <- t])
   EIPlus (cat,t) -> EIPlus (updECat ii cat, [recss ii 0 s | s <- t])
   EIOpt  (cat,t) -> EIOpt  (updECat ii cat, [recss ii 0 s | s <- t])
   _ -> it
  
pickERules :: NormERule a -> [NormERule a]
pickERules rule@(cat,alts) = rule : concat (map pics (concat alts)) where
 pics it = case it of
   EIStar ru@(cat,t) -> mkEStarRules cat ++ pickERules ru
   EIPlus ru@(cat,t) -> mkEPlusRules cat ++ pickERules ru
   EIOpt  ru@(cat,t) -> mkEOptRules cat ++ pickERules ru
   _ -> []
 mkEStarRules cat = [(cat', [[],[EINonTerm cat, EINonTerm cat']])] 
                                        where cat' = mkNewECat cat "Star"
 mkEPlusRules cat = [(cat', [[EINonTerm cat],[EINonTerm cat, EINonTerm cat']])] 
                                        where cat' = mkNewECat cat "Plus"
 mkEOptRules cat  = [(cat', [[],[EINonTerm cat]])] 
                                        where cat' = mkNewECat cat "Opt"

nubERules :: Eq a => [NormERule a] -> [NormERule a]
nubERules rules = nub optim where 
  optim = map (substERules (map mkSubst replaces)) irreducibles
  (replaces,irreducibles) = partition reducible rules
  reducible (cat,[items]) = isNewCat cat && all isOldIt items
  reducible _ = False
  isNewCat (_,ints) = ints == []
  isOldIt (EITerm _) = True
  isOldIt (EINonTerm cat) = not (isNewCat cat)
  isOldIt _ = False
  mkSubst (cat,its) = (cat, head its) -- def of reducible: its must be singleton
--- the optimization assumes each cat has at most one EBNF rule.

substERules :: [(ECat,[EItem a])] -> NormERule a -> NormERule a
substERules g (cat,itss) = (cat, map sub itss) where
  sub [] = []
  sub (i@(EINonTerm cat') : ii) = case lookup cat g of
                                    Just its -> its ++ sub ii 
                                    _ -> i : sub ii
  sub (EIStar r : ii) = EIStar (substERules g r) : ii
  sub (EIPlus r : ii) = EIPlus (substERules g r) : ii
  sub (EIOpt r : ii)  = EIOpt  (substERules g r) : ii

eitem2cfitem :: EItem a -> CFItem a String 
eitem2cfitem it = case it of
  EITerm a -> mkCFTerm a
  EINonTerm cat  -> CFNonterm (mkCFCat cat)
  EIStar (cat,_) -> CFNonterm (mkCFCat (mkNewECat cat "Star"))
  EIPlus (cat,_) -> CFNonterm (mkCFCat (mkNewECat cat "Plus"))
  EIOpt  (cat,_) -> CFNonterm (mkCFCat (mkNewECat cat "Opt"))

type NormERule a = (ECat,[[EItem a]]) -- disjunction of sequences of items

data EItem a =
   EITerm a
 | EINonTerm (ECat)
 | EIStar (NormERule a)
 | EIPlus (NormERule a)
 | EIOpt  (NormERule a)
  deriving Eq

normERule :: ([Int],ERule a) -> NormERule a
normERule (ii,(cat,rhs)) = 
 (cat,[map (mkEItem (ii ++ [i])) r' | (i,r') <- zip [0..] (disjNorm rhs)]) where
  disjNorm r = case r of
    ESeq r1 r2 -> [x ++ y | x <- disjNorm r1, y <- disjNorm r2]
    EAlt r1 r2 -> disjNorm r1 ++ disjNorm r2
    EEmpty -> [[]]
    _ -> [[r]]

mkEItem :: [Int] -> ERHS a -> EItem a
mkEItem ii rhs = case rhs of
  ETerm a -> EITerm a
  ENonTerm cat -> EINonTerm cat
  EStar r -> EIStar (normERule (ii,(mkECat ii, r)))
  EPlus r -> EIPlus (normERule (ii,(mkECat ii, r)))
  EOpt  r -> EIOpt  (normERule (ii,(mkECat ii, r)))
  _ -> EINonTerm ("?????",[])
--  _ -> error "should not happen in ebnf" ---

mkECat ints = ("C", ints)

prECat (c,[]) = c
prECat (c,ints) = c ++ "_" ++ prTList "_" (map show ints)

mkCFCat ci = (prECat ci)

updECat _ (c,[]) = (c,[])
updECat ii (c,_) = (c,ii)

mkNewECat (c,ii) str = (c ++ str,ii)

------ parser for EBNF grammars, "Java style" but notice semicolons

pEBNF :: Token a => Parser Char (EBNF a)
pEBNF = longestOfMany (pJ pERule)

pERule :: Token a => Parser Char (ERule a)
pERule = pECat ... jL ":=" +.. pERHS 0 ..+ jL ";"

pERHS :: Token a => Int -> Parser Char (ERHS a)
pERHS 0 = pTList "|" (pERHS 1) *** foldr1 EAlt
pERHS 1 = longestOfMany (pJ (pERHS 2)) *** foldr ESeq EEmpty
pERHS 2 = pERHS 3 ... pJ pUnaryEOp *** (\ (a,f) -> f a)
pERHS 3 = parseTok *** ETerm ||| pECat *** ENonTerm ||| pParenth (pERHS 0)

pUnaryEOp :: Token a => Parser Char (ERHS a -> ERHS a)
pUnaryEOp = 
 lits "*" <<< EStar ||| lits "+" <<< EPlus ||| lits "?" <<< EOpt ||| succeed id

pECat = pIdent *** (\c -> (c,[]))

