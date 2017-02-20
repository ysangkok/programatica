module PrGrammar where

import Operations
import Grammar
import Tokens

-- AR 7/12/1999 - 1/4/2000

-- printing and prettyprinting class

class Print a where
 prt         :: a -> String                -- one line
 prettyPrt   :: a -> String                -- many lines
 prl         :: Int -> a -> [(Int,String)] -- indented lines
 prl i a = pr0 i (prt a)
 prettyPrt a = unlines [indent i s | (i,s) <- prl 0 a]
 prt = concat . (map snd) . (prl 0)

pr0 :: Int -> String -> [(Int,String)] 
pr0 i s = [(i,s)] -- don't add indentation

-- to show terms etc in error messages
prtBad :: Print a => String -> a -> Err b
prtBad s a = Bad (s +++ prt a)

-- instances for Grammar data types

instance Token a => Print (Term a) where  --- improve pretty-printing !
 prt trm =
  case trm of
    Var s -> prt s
    Cons s -> prt s
    Literal _ s -> s
    App c a -> prt1 c +++ prt2 a
    Abs x b -> "\\" ++ prt x +++ "->" +++ prt b
    Meta m  -> prt m
    Prod x a b -> prDecl x a +++ "->" +++ prt b
    TypeType -> "Type"
    Closure g t -> prt2 t ++ prCurlyList [prt x +++ ":=" +++ prt a | (x,a) <- g]
    Typed a t -> "<" ++ prt a +++ ":" ++ prt t ++ ">" ---
    Record r -> prCurlyList [prt l +++ "=" +++ prt a | (l,a) <- r]
    RecType r -> prCurlyList [prt l +++ ":" +++ prt a | (l,a) <- r]
    UpdRecord t (l,v) -> prt t +++ "**" +++ prCurly (prt l +++ "=" +++ prt v)
    UpdRecType t (l,v) -> prt t +++ "*" +++ prCurly (prt l +++ ":" +++ prt v)
    Project t l -> prt t ++ "." ++ prt l
    Cases cc -> "table" +++ prCurlyList (map prCase cc)
    Table [x] v -> prt2 x +++ "=>" +++ prt v
    Table xx v -> prArgList (map prt xx) +++ "=>" +++ prt v
    Select f [x] -> prt1 f +++ "!" +++ prt2 x
    Select f xx -> prt1 f +++ "!" +++ prArgList (map prt xx)
    Let dd t -> "let" +++ 
                prCurlyList 
                  [prt x +++ ":" +++ prt a +++ "=" +++ prt b | (x,a,b) <- dd] +++
                "in" +++ prt t
    Tok a | isZeroTok a -> "[]"
    Tok a -> prTok a
    Concat a b -> prt1 a +++ "++" +++ prt b
    ArgVar (c,(d,i)) -> "<" ++ prt c ++ "_" ++ show d ++ "_" ++ show i ++ ">"
    TypeStr -> "Str"
    TypeStrs -> "Strs"
    LiT a -> "Lin" +++ prt2 a
    Glue a b -> prt1 a +++ "+" +++ prt b
    Alts DPrefix (t, tt) -> "pre" +++ 
                 prCurlyList (prt t : [prt y +++ "/" +++ prt x | (x,y) <- tt])
    Alts DPostfix (t, tt) -> "post" +++ 
                 prCurlyList (prt t : [prt y +++ "/" +++ prt x | (x,y) <- tt])
    Strs tt -> "strs" +++ prCurlyList (map prt tt)  
--- prl         :: Int -> a -> [(Int,String)] -- indented lines
--- prl i t =
---  case t of
    

prt2 t =
 case t of
   App _ _ -> prParenth $ prt t
   LiT _ -> prParenth $ prt t
   Cases _  -> prParenth $ prt t
   Alts _ _  -> prParenth $ prt t
   Strs _  -> prParenth $ prt t
   Project _ _  -> prParenth $ prt t
   _ -> prt1 t

prt1 t =
 case t of
   Abs _ _ -> prParenth $ prt t
   Prod _ _ _ -> prParenth $ prt t
   Table _ _ -> prParenth $ prt t
   Select _ _ -> prParenth $ prt t
   Concat _ _ -> prParenth $ prt t
   Let _ _ -> prParenth $ prt t
   Glue _ _ -> prParenth $ prt t
   Closure _ _ -> prParenth $ prt t
   UpdRecType _ _ -> prParenth $ prt t
   UpdRecord _ _ -> prParenth $ prt t
   _ -> prt t

instance Print Patt where
  prt p = case p of
    PattVar s -> prt s
    PattCons c [] -> prt c
    PattCons c a -> prt c +++ unwords (map prtp2 a)
    PattRec r -> prCurlyList [prt l +++ "=" +++ prt a | (l,a) <- r]
   where
     prtp2 p = case p of
       PattCons _ (_:_) -> prParenth (prt p)
       _ -> prt p

instance Print Ident where prt (Ident (s,(n,p))) = s ---- s
                              ---- s ++ "-" ++ show n ++ "-" ++ show p ----
instance Print Label where 
  prt (Label (s,0)) = s
  prt (Label (s,i)) = s ++ show i
instance Print MetaSymb where 
  prt (MetaSymb (s,i)) = "[?" ++ prt s ++ replicate i '\'' ++ "?]"

instance Print Def where
 prt d =
  case d of
    DefCat cat cont -> "cat" +++ prt cat +++ prContext cont
    DefType typ t -> "type" +++ prt typ +++ prt t
    DefFun fun typ -> "fun" +++ prt fun +++ ":" +++ prt typ
    DefDef patt trm -> "def" +++ prt patt +++ "=" +++ prt trm
    DefData c cc -> "data" +++ prt c +++ "=" +++ prTList " | " (map prt cc)
  +++ ";"

instance Token a => Print (LDef a) where
 prt d =
  case d of
    DefParam tagt params -> 
      "param" +++ prt tagt +++ "=" +++ prTList " | " (map prParam params) where
         prParam (tag, cont) = prt tag +++ prParamContext cont
    DefOper op typ t -> 
      "oper" +++ prt op +++ ":" +++ prt typ +++ "=" ++++ "  " ++ prt t
    DefLType typ t -> "lintype" +++ prt typ +++ "=" +++ prt t
    DefLintype typ t -> "lincat" +++ prt typ +++ "=" +++ prt t
    DefDefault fun args t -> 
      "lindef" +++ prt fun +++ unwords (map prt args) +++ "=" ++++ "  " ++ prt t
    DefLin fun args t -> 
      "lin" +++ prt fun +++ unwords (map prt args) +++ "=" ++++ "  " ++ prt t
    DefVar x cats -> 
      "var" +++ prt x +++ ":" +++ prTList "," (map prt cats) 
    DefTokenizer t -> 
      "tokenizer" +++ prt t
  +++ ";"

instance Token a => Print (Grammar a) where
  prt (Grammar (abs,conc)) = prt abs +++++ prt conc

instance Print Abstract where prt (Abstract defs) = unlines (map prt defs)

instance Token a => Print (Concrete a) where
  prt (Concrete ldefs) =  unlines (map prt ldefs)

prContext :: Token a => Context a -> String
prContext cont = unwords ["(" ++ prt x ++ ":" ++ prt typ ++ ")" | (x,typ) <- cont]

prParamContext :: Token a => Context a -> String
prParamContext = unwords . map (prt . snd) --- no dep. types in param context

prCase ([p],t) = prt p +++ "=>" +++ prt t
prCase (pp,t)  = prArgList (map prt pp) +++ "=>" +++ prt t

prDecl x a = case x of
  Ident ("_",_) -> prt2 a
  _ -> prParenth (prt x +++ ":" +++ prt a)

prPattList :: [[Patt]] -> String
prPattList pps = prSemicList (map (prTList "," . map prt) pps) +++ ": "

