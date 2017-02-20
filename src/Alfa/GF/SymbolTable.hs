module SymbolTable where

import Operations
import Tokens
import Grammar
import PrGrammar
import Macros
import List (partition)

-- symbol tables = binary search trees. AR 23/1/2000 -- 31/3

-- the data structures used for storing grammar in search trees

type GrammarST a = (AbstractST, ConcreteST a)
type AbstractST = (BinTree (Ident,IdentInfo),Int)
type ConcreteST a = (BinTree (Ident,IdentLInfo a),Int)

emptyGrammarST :: GrammarST a
emptyGrammarST = (emptyAbstractST,emptyConcreteST)
emptyAbstractST = (NT, 0::Int)
emptyConcreteST = (NT, 0::Int)
onlyAbsST abs = (abs, emptyConcreteST)

data IdentInfo = 
   IdentCat Cont
 | IdentType Type
 | IdentFun Type 
 | IdentDef [(Patt,Trm)]
 | IdentData [Ident]
  deriving (Read,Show)

data IdentLInfo a = 
   IdentTagType [Param a]
 | IdentTag (LTerm a)
 | IdentOper (LTerm a) (LTerm a)
 | IdentLType (LTerm a)
 | IdentLintype (LTerm a)         --- these two should be one
 | IdentDefault [Ident] (LTerm a)
 | IdentLin [Ident] (LTerm a)
 | IdentVar [Cat]
 | IdentTokenizer Ident
  deriving (Read,Show)

-- from definitions to ident infos

mkAdHocIdent c = zIdent ("@" ++ symid c) --- avoid by better design of Info types!
stripAdHocIdent c = zIdent (tail (symid c))

mkIdentInfos :: [Def] -> [(Ident,IdentInfo)]
mkIdentInfos (def:dd) = case def of
   DefCat cat context  -> (cat, IdentCat context) : mkIdentInfos dd
   DefType ident typ   -> (ident, IdentType typ)  : mkIdentInfos dd
   DefFun fun typ      -> (fun, IdentFun typ)     : mkIdentInfos dd
   DefDef patt trm     ->  
       (mkAdHocIdent fun,IdentDef (dp:dd1)) : mkIdentInfos dd2
       --- not to get confused when looking up the type of fun
         where
           fun = funOfPatt patt
           funOfPatt (PattCons f _) = f

           funOfPatt (PattVar f) = f    --- workaround to a bug in Rename
           rp p = case p of
                    PattVar f -> PattCons f []
                    _ -> p

           dp  = (rp patt,trm)
           dd1 = [(rp p,t) | DefDef p t <- dd0]
           (dd0,dd2) = partition df dd
           df (DefDef p _) = eqStrIdent fun (funOfPatt p)
           df _ = False
   --- collecting definitions for one fun together
   DefData c cc -> 
       (mkAdHocIdent c,IdentData cc)     : mkIdentInfos dd
   _ -> []
mkIdentInfos [] = []

mkIdentLInfos :: [LDef a] -> [(Ident,IdentLInfo a)]
mkIdentLInfos = concat . map mkIdentLInfo 
  where
    mkIdentLInfo ldef = case ldef of
     DefParam pt params -> 
       (pt, IdentTagType params) :
       [(tag, IdentTag (mkProd (args, Var pt, []))) | (tag, args) <- params]
     DefOper op ty t -> (op, IdentOper ty t) : []
     DefLType ident ty -> (ident, IdentLType ty) : []
     DefLintype cat ty -> (cat, IdentLintype ty) : []
     DefDefault cat xx t -> (mkAdHocIdent cat, IdentLin xx t) : []
     DefLin fun xx t -> (fun, IdentLin xx t) : []
     DefVar var cats -> (var, IdentVar cats) : []
     DefTokenizer tok -> (zIdent ("tokenizer"), IdentTokenizer tok) : []
                                -- thus no clash betw idents/tokenizer names
-- print lookup trees

prGrammarST :: Token a => GrammarST a -> String
prGrammarST (abs, conc) = prST abs +++++ prST conc

prST :: (Print a, Print b) => (BinTree (a,b),Int) -> String 
prST (tr,m) = unlines ((map (\ (t,i) -> prt t +++ "::" +++ prt i)) (tree2list tr))
               +++++ "--" +++ show (length (tree2list tr)) +++ "constants"
               +++++ "--" +++ show m +++ "maximal index"

instance Print IdentInfo where
 prt d =
  case d of
    IdentCat cont -> "Cat" +++ prContext cont
    IdentFun typ  -> prt typ
    IdentType typ -> prt typ
    IdentDef ppt  -> unlines [prt p +++ "=" +++ prt t | (p,t) <- ppt] 
    IdentData cc  -> prTList " | " (map prt cc)
  +++ ";"

instance Token a => Print (IdentLInfo a) where
 prt d =
  case d of
    IdentTagType params -> 
      "ParType" +++ "=" +++ prTList " | " (map prParam params) where
         prParam (tag, cont) = prt tag +++ prContext cont
    IdentTag typ -> prt typ
    IdentOper typ t -> prt typ +++ "=" +++ prt t
    IdentLType typ -> "Lintype" +++ "=" +++ prt typ
    IdentLintype typ -> "Lincat" +++ "=" +++ prt typ
    IdentLin [] t -> "lin" +++ "=" +++ prt t
    IdentLin args t -> "lin" +++ "= \\" +++ unwords (map prt args) +++ "->"+++ prt t
    IdentVar cats -> "var" +++ prTList "," (map prt cats) 
    IdentTokenizer t -> "tokenizer" +++ prt t
  +++ ";"

