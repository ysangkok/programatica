module GFCFIdents where

import Grammar
import Tokens ()
import PrGrammar --- (prt)
import CF
import Macros

-- AR 11/2/2000 -- 30/3 -- 26/1/2001

-- datatypes used for parsing GF

type GFCFCat    = String -- for simplicity
type GFCF a     = CF     a GFCFCat GFCFFun
type GFCFTree   = CFTree GFCFCat GFCFFun
type GFCFRule a = CFRule a GFCFCat GFCFFun
type GFCFItem a = CFItem a GFCFCat

type Binds   = [Int]
type Profile = [(Cat,[Int])]
newtype GFCFFun = 
  GFCFFun ((Trm, Binds),Profile) -- the Trm is Cons | Meta | Var | Predefined
    deriving (Eq,Show)

instance Ord GFCFFun where f < f' = f == f' --- needed for CP : remove later!

mkGFFun :: Trm -> GFCFFun
mkGFFun t = GFCFFun ((t,[]),[])

instance CFIdent GFCFFun where
  prCFIdent (GFCFFun ((t, b),p)) = prt t ++ pb b ++ pp p where
    pb b = if null b then "" else "-" ++ concat (map show b)
    pp p = if normal p then "" else "_" ++ concat (map (show . snd) p)
    normal p = and [x==y | (x,y) <- zip (map snd p) (map (:[]) [0..])]
  mkCFIdent = mkGFFun . Var . mkIdent "%" --- needed for CP

mkGFTree :: GFCFCat -> Trm -> GFCFTree
mkGFTree c = mkCFTree c . mkGFFun

catVarGFCF = "#Var" --- :: GFCFCat

mkGFCFCat = symid --- :: Cat -> GFCFCat

mkGFCFCatDisc i c = if i==0 then c else c ++ "#" ++ show i -- make discont cat symb

-- we need this intermediate structure to compute profiles in GFtoCF
data PreGFCFItem a = PTerm (RegExp a) | PNonterm GFCFCat Int deriving Eq

precf2cf (PTerm r) = CFTerm r
precf2cf (PNonterm c i) = CFNonterm c

isRuleForFun f (GFCFFun ((Cons f',_),_),_) = eqStrIdent f f'
isRuleForFun _ _ = False

