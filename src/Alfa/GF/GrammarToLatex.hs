module GrammarToLatex (grammar2latex, mkLinLatex, gr2tex, trm2tex) where

import Operations
import Grammar
import Tokens
import List (nub, groupBy, intersperse)

infixr 5 +!+
infixr 5 +*+

-- AR 28/11/2000

-- printing grammars in LaTeX

grammar2latexfile :: (Token a) => Grammar a -> String
grammar2latexfile g = begindocument +++++ macros +++++ prt g +++++ enddocument

grammar2latex :: (Token a) => Grammar a -> String
grammar2latex g = settings +++++ macros +++++ prt g

gr2tex  :: (Token a) => Grammar a -> String
gr2tex = prt

trm2tex :: (Token a) => Term a -> String
trm2tex = prt

settings = "\\setlength{\\parskip}{2mm}\\setlength{\\parindent}{0mm}\\scriptsize"

-- auxiliaries

keyw w    = "{\\keyw{" +++ w ++ "}}"

symb w    = "{\\mbox{\\syb{" ++ w ++ "}}}" -- identifiers in roman
string w  = "{\\mbox{\\str{" ++ w ++ "}}}" -- tokens in italics, without quotes

macros =
 "\\newcommand{\\keyw}[1]{\\mbox{{\\bf #1 }}}" ++++
 "\\newcommand{\\syb}[1]{{\\rm #1}}" ++++
 "\\newcommand{\\str}[1]{{\\em #1}}" ++++
 "\\newcommand{\\kwtable}{\\mbox{{\\bf table}}}"

x +!+ y   = x +++ "\\;" +++ y
layout [c] = "\\{" +!+ c +!+ "\\}"
layout cs  = "\\left\\{\n  \\begin{array}{l}" ++++ 
             unlines (map (+++ "\\\\") cs)    ++++
             "\\end{array}\\right\\}"

layoutE e [(c,d)] = "\\{" +++ c +++ e +++ d +++ "\\}"
layoutE e cs  = "\\left\\{\n  \\begin{array}{lcl}" ++++ 
                unlines [c +++ "&" +++ e +++ "&" +++ d +++ "\\\\" | (c,d) <- cs]++++
                "\\end{array}\\right\\}"

colon  = "\\;:\\;"
equal  = "\\;=\\;"
scolon = "\\,:\\," -- short-space versions
sequal = "\\,=\\,"
mathenv s = "\n\\(" ++++ s ++++ "\\)\n"
subindex x i = x ++ "_{" ++ i ++ "}"

breakpoint = "\\)\\("
x +*+ y = x +++ breakpoint +++ y -- N.B. may not be inside layout list!

class Print a where prt :: a -> String

-- instances for Grammar data types

instance Token a => Print (Term a) where
 prt trm =
  case trm of
    Var s -> prt s
    Cons s -> prt s
    Literal _ s -> s
    App c a -> prt1 c +!+ prt2 a
    Abs x b -> "\\lambda" ++ prt x +!+ "\\rightarrow" +!+ prt b
    Meta m  -> prt m
    Prod x a b -> prArgDecl x a +!+ "\\rightarrow" +!+ prt b
    TypeType -> symb "Type"
    Closure g t -> prt2 t ++ prCurlyList [prt x +++ equal +++ prt a | (x,a) <- g]
    Typed a t -> "<" ++ prt a +++ colon +++ prt t ++ ">" ---
    Record r -> layoutE "=" [(prt l, prt a) | (l,a) <- r]
    RecType r -> layoutE ":" [(prt l, prt a) | (l,a) <- r]
    Project t l -> prt2 t ++ "." ++ prt l
    Cases cc -> "\\kwtable" +!+ layoutE "\\Rightarrow" (map prCase cc)
    Table [x] v -> prt2 x +!+ "\\Rightarrow" +!+ prt v
    Table xx v -> prArgList (map prt xx) +!+ "\\Rightarrow" +!+ prt v
    Select f [x] -> prt1 f +!+ "!" +!+ prt2 x
    Select f xx -> prt1 f +!+ "!" +!+ prArgList (map prt xx)
    Let dd t -> keyw "let" +!+ 
                layout 
                 [prt x +++ colon +++ prt a +++ equal +++ prt b | (x,a,b) <- dd] +!+
                keyw "in" +!+ prt t
    Tok a | isZeroTok a -> "\\mbox{[ ]}"
    Tok a -> string (showTok a)
    Concat a b -> prt1 a +!+ "+\\!\\!+" +!+ prt b
    ArgVar (c,(d,i)) -> "<" ++ prt c ++ "\\_" ++ show d ++ "\\_" ++ show i ++ ">"
    TypeStr -> symb "Str"
    TypeStrs -> symb "Strs"
    LiT a -> keyw "Lin" +!+ prt2 a
    Glue a b -> prt1 a +!+ "+" +!+ prt b
    Alts DPrefix (t, tt) -> keyw "pre" +!+ 
                 layout (prt t : [prt y +++ "/" +++ prt x | (x,y) <- tt])
    Alts DPostfix (t, tt) -> keyw "post" +++ 
                 layout (prt t : [prt y +++ "/" +++ prt x | (x,y) <- tt])
    Strs tt -> keyw "strs" +!+ layout (map prt tt)  
    
prt2 t =
 case t of
   App _ _ -> prParenth $ prt t
   LiT _ -> prParenth $ prt t
   Cases _  -> prParenth $ prt t
   Alts _ _  -> prParenth $ prt t
   Strs _  -> prParenth $ prt t
   Select _ _ -> prParenth $ prt t ---
   _ -> prt1 t

prt1 t =
 case t of
   Abs _ _ -> prParenth $ prt t
   Prod _ _ _ -> prParenth $ prt t
   Table _ _ -> prParenth $ prt t
---   Select _ _ -> prParenth $ prt t
   Concat _ _ -> prParenth $ prt t
   Let _ _ -> prParenth $ prt t
   Glue _ _ -> prParenth $ prt t
   _ -> prt t

instance Print Patt where
  prt p = case p of
    PattVar s -> prt s
    PattCons c [] -> prt c
    PattCons c a -> prt c +!+ prTList "\\," (map prtp2 a)
    PattRec r -> layoutE "=" [(prt l, prt a) | (l,a) <- r]
   where
     prtp2 p = case p of
       PattCons _ (_:_) -> prParenth (prt p)
       _ -> prt p

instance Print Ident where 
  prt (Ident ("_",_)) = "\\_"
  prt (Ident (s,(n,p))) = symb s

instance Print Label where 
  prt (Label (s,0)) = symb s
  prt (Label (s,i)) = subindex (symb s) (show i)
instance Print MetaSymb where 
  prt (MetaSymb (s,0)) = 
    "?_{\\tiny" +++ symb (prt s) ++ "}"
  prt (MetaSymb (s,i)) = 
    "?_{\\tiny" +++ symb (prt s) ++ "}_{\\scriptsize" ++ show i ++ "}}"

instance Print Def where
 prt d =
  case d of
    DefCat cat cont -> keyw "cat" +!+ prt cat +!+ prContext cont
    DefType typ t -> keyw "type" +!+ prt typ +++ equal +++ prt t
    DefFun fun typ -> keyw "fun" +!+ prt fun +++ colon +++ prt typ
    DefDef patt trm -> keyw "def" +!+ prt patt +++ equal +++ prt trm
    DefData c cc -> 
      keyw "data" +!+ prt c +++ equal +++ prTList "\\;\\mid\\;" (map prt cc) +++ ";"

instance Token a => Print (LDef a) where
 prt d =
  case d of
    DefParam tagt params -> 
      keyw "param" +!+ prt tagt +++ equal +++ 
      prTList "\\;\\mid\\;" (map prParam params) where
         prParam (tag, cont) = prt tag +!+ prParamContext cont
    DefOper op typ t -> 
      keyw "oper" +!+ prt op +++ colon +++ prt typ +++ equal +*+ prt t
    DefLType typ t -> keyw "lintype" +!+ prt typ +++ equal +++ prt t
    DefLintype typ t -> keyw "lincat" +!+ prt typ +++ equal +++ prt t
    DefDefault fun args t -> 
      keyw "lindef" +!+ prt fun +!+ prTList "\\," (map prt args) +++ 
      equal +*+ prt t
    DefLin fun args t -> 
      keyw "lin" +!+ prt fun +!+ prTList "\\," (map prt args) +++ 
      equal +*+ prt t
    DefVar x cats -> 
      keyw "var" +!+ prt x +++ colon +++ prTList "," (map prt cats) 
    DefTokenizer t -> 
      keyw "tokenizer" +!+ prt t 
-----  +++ ";"

instance Token a => Print (Grammar a) where
  prt (Grammar (abs,conc)) = prt abs +++++ prt conc

instance Print Abstract where 
  prt (Abstract defs) = unlines (map (mathenv . prt) defs)

instance Token a => Print (Concrete a) where
  prt (Concrete ldefs) = unlines (map (mathenv . prt) ldefs)

prContext :: Token a => Context a -> String
prContext cont = unwords $ intersperse "\\;" [prDecl x typ | (x,typ) <- cont]

prParamContext :: Token a => Context a -> String
prParamContext = unwords . intersperse "\\;" . map (prt . snd) --- no dep. types

prCase ([p],t) = (prt p,prt t)
prCase (pp,t)  = (prArgList (map prt pp), prt t)

prDecl x a = case x of
  Ident ("_",_) -> prt1 a
  _ -> prParenth (prt x +++ colon +++ prt a)
prArgDecl x a = case x of
  Ident ("_",_) -> prt1 a
  _ -> prParenth (prt x +++ scolon +++ prt a)

prPattList :: [[Patt]] -> String
prPattList pps = prSemicList (map (prTList "," . map prt) pps) +++ ": "


-- print linearization results as inflection tables in LaTeX

mkLinLatex :: [([String],String)] -> String
mkLinLatex rs = case dpth of
    0 -> foldr1 (+++++) $ map (eex . snd) rs  
    1 -> tabular 
           "{|l|l|}\n\\hline\n" 
           [p +++ "&" +++ eex s +++ "\\\\" | (p:_, s) <- rs]
    2 -> tabular 
           ("{|l|l|l|}\n\\hline\n" ++++ 
                unwords (map ("&" +++) (cols2 rs)) +++ "\\\\ \\hline")
           (prCol2 rs)
    n -> foldr1 (+++++) allTabs
  where
    dpth  = if null rs then 0 else length $ fst $ head rs
    cols2 ts = nub [prp p | (ps,_) <- ts, let p = reverse ps !! 1] -- length ps > 1
    tabular h ss = "\\begin{tabular}" ++ h ++++ unlines ss ++++ 
                   "\\hline\n\\end{tabular}"
    eex s = "{\\em" +++ s ++ "}"
    allTabs = map prOne $ groupBy (\ (x,_) (y,_) -> take ps x == take ps y) rs
                where 
                  ps = dpth - 2
                  prOne t@((x,s):_) = 
                     unwords (take ps x) +++++ 
                     mkLinLatex [(drop ps x,s) | (x,s) <- t]
    prCol2 ts = prRows $ 
                  groupBy (\ (x,_) (y,_) -> head x == head y) rs
                    where
                      prRows g@(rr:_) = 
                        [prOne (last (fst (rr !! i)), map (snd . (!!i)) g) | 
                                                          i <- [0..length rr -1]]
                      prOne (p,ss) =
                        prp p +++ unwords (map (("&" +++) . eex) ss) +++ "\\\\"

    prp pp = pp

