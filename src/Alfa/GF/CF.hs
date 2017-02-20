module CF where
-- import Grammar (Ident)
import PrGrammar 
import List (nub,nubBy)

-- AR 15/12/1999 -- 30/3/2000

-- context-free grammar with token type a, identifier type c
-- put a = c = String, literal function to (const []) for simple applications

{- -- bug workaround for nhc98-1.00 /TH
#ifdef __NHC__
#define newtype data
#endif
-}

-- cf grammar data types
type CF a c f = ([CFRule a c f], CFPredef a c f) 
type CFCat c = c -- for simplicity
type CFFun f = f
data CFItem a c   = CFTerm (RegExp a) | CFNonterm c deriving (Eq,Show)
type CFRule a c f = (f, (c, [CFItem a c]))
newtype CFTree c f = CFTree (f,(c, [CFTree c f])) deriving Eq
type CFPredef a c f = a -> [(c,f)] -- recognize literals, variables, etc

-- basic grammar operations
emptyCF = ([], emptyCFPredef)
emptyCFPredef = const []
unionCF (r1,f1) (r2,f2) = (r1 ++ r2, \s -> nub (f1 s ++ f2 s))

predefOfCF :: CF a c f -> CFPredef a c f
predefOfCF = snd

-- make a tree without constituents
mkCFTree :: c -> f -> CFTree c f
mkCFTree c f = CFTree (f,(c,[]))

-- make a rule from a single token without constituents
mkCFRule :: c -> f -> a -> CFRule a c f
mkCFRule c f a = (f,(c,[CFTerm (mkRegTok a)]))

-- terminals can be regular expressions on [a]
data RegExp a = RegAlts [a] --- to be completed to full regexp
  deriving (Eq,Ord,Show) -- Ord for CP ; makes no sense otherwise

mkRegTok t = RegAlts [t]
mkCFTerm = CFTerm . mkRegTok
wordsOfRegExp (RegAlts tt) = tt

satRegExp :: Eq a => RegExp a -> a -> Bool
satRegExp r t = case r of RegAlts tt -> elem t tt

-- methods that category symbols should have
class (Show c, Eq c) => CFIdent c where
 compatCF  :: c -> c -> Bool
 prCFIdent :: c -> String
 mkCFIdent :: Int -> c -- to provide a supply of new idents
 compatCF  = (==)
 prCFIdent = show

instance CFIdent Char where  
  compatCF = (==)
  prCFIdent c = [c]
  mkCFIdent i = toEnum i

instance CFIdent a => CFIdent [a] where 
  compatCF = (==)
  prCFIdent = concat . (map prCFIdent)
  mkCFIdent i = [mkCFIdent i]

-- operations on cf grammars

allCFCats :: CFIdent c => CF a c f -> [c]
allCFCats (rr,_) = 
  nubBy compatCF [c | (_,(v,its)) <- rr, c <- v:[x | CFNonterm x <- its]]

rulesFor :: CFIdent c =>  CF a c f -> c -> [CFRule a c f]
rulesFor (cfg,_) c = [r | r@(_,(c',_)) <- cfg, compatCF c' c]

startCat :: CF a c f -> c
startCat = fst . snd . head . fst

type Toks a = [(Int,a)]

mkToks :: [a] -> Toks a
mkToks = zip [0..]

forItem :: (Eq a, CFIdent c) => a -> CFRule a c f -> Bool
forItem a (_,(_, CFTerm t : _)) = satRegExp t a
forItem _ _ = False

isCircularCF :: (Eq a, CFIdent c) => CFRule a c f -> Bool
isCircularCF (_,(c', CFNonterm c:[])) = compatCF c' c
isCircularCF _ = False

-- predef for a given category
predefForCat ::  CFIdent c => CFPredef a c f -> c -> a -> [CFTree c f]
predefForCat pre cat s = [mkCFTree c f | (c,f) <- pre s, compatCF c cat]

-- coercion to the older predef cf type
predefRules :: CFIdent c => CFPredef a c f -> a -> [CFRule a c f]
predefRules pre s = [mkCFRule c f s | (c,f) <- pre s]

