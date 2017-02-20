-- A new version of PGrammar which takes advantage of a lexical analyser
module PGrammar2(
  pGrammar,pImports,
  GParser,ParseToken(..),Imports,
  pMeta, pTrm, pTrm', pConstraints
 ) where

import Operations
import Parsers hiding (jL,pJ,pJunk,pIdent,pVarIdent,pFileName,pParenth,pArgList2,pCommaList,pTList,(....))
import LexGrammar
import LexPos
import Tokens
import Grammar
import Macros
import Predefined hiding (pLiteral)

import Char(isAlpha,isDigit)
--import Trace(trace)
infixr 5 ....

{-{- -- Bug workaround for nhc98 /TH
#ifdef __NHC__
#define DEFAULT -} default(Str) {-
#endif
-}-}
{-DEFAULT-}

type GParser a = Parser PToken a

class Token a => ParseToken a where
  gParseTok :: GParser a
  gParseToks :: GParser [a]

instance ParseToken () where
  gParseTok = fails "<() token>"
  gParseToks = fails "<() tokens>" --longestOfMany gParseTok

instance ParseToken a => ParseToken [a] where
  gParseTok = gParseTok *** (:[])
  gParseToks = gParseToks *** (:[])

instance ParseToken Str where
  gParseTok = strlit *** Str
  gParseToks = sep '[' +.. strlit ..+ sep ']' *** (map Str . words)

-- grammar flags : imports
-- imports : list of grammar file names
type Imports = [String]
pImports = 
  kw "include" +.. pSemicList pFileName ..+ semi +|| succeed []
  -- "import" is obsolete: this is more like an include operation

-- previously one, two, or more colons...
pColons = kw ":"

-- the main function

pGrammar :: ParseToken a => GParser (Imports, Grammar a)
pGrammar = 
  pImports ...
  (longestOfMany (pJ pDefinitions) *** mkGrammar)
   where
    mkGrammar dd = Grammar (Abstract (reverse defs), Concrete (reverse ldefs)) where
      (defs,ldefs) = sortDefs ([],[]) (concat dd)
      sortDefs g@(abs,conc) d = case d of
        [] -> g
        Left d : ds -> sortDefs (d:abs, conc) ds
        Right d : ds -> sortDefs (abs, d:conc) ds

pDefinitions :: ParseToken a => GParser [Either Def (LDef a)]
pDefinitions =
   pDef *** map Left +|| pLDef *** map Right +|| pDefMacro *** mkD
 where
   mkD dd = concat [[Left def, Right ldef] | (def,ldef) <- dd]

pDef :: GParser [Def]
pDef =
  kw "cat" +.. pAllSemicList (pVIdent .... pContext *** uncurry DefCat)
 +||
  kw "fun" +.. pAllSemicList 
                  (someSep (kw ",") pVIdent ... pColons +.. pTerm *** 
                                (\ (fs,t) -> [DefFun f t | f <- fs])) 
        *** concat
 +||
  kw "type" +.. pAllSemicList (pVIdent ... kw "=" +.. pTerm *** uncurry DefType)
 +||
  kw "def" +.. pAllSemicList (pPatt ... kw "=" +..  pTerm *** uncurry DefDef)
 +||
  kw "data" +.. pAllSemicList (pVIdent ... kw "=" +..  pConstrs *** uncurry DefData)
--}
pLDef :: ParseToken a => GParser [LDef a]
pLDef =
  kw "param" +.. pAllSemicList (pVIdent ... kw "=" +.. pParams *** uncurry DefParam)
 +||
  kw "oper" +.. pAllSemicList (pVIdent ... pColons +.. pTerm ... kw "=" +.. pTerm 
                             *** (\ (c,(a,b)) -> DefOper c a b))
 +||
  kw "lintype" +.. pAllSemicList (pVIdent ... kw "=" +.. pTerm *** uncurry defLType)
 +||
  kw "lincat" +.. pAllSemicList (pVIdent ... kw "="+.. pTerm *** uncurry DefLintype)
 +||
  kw "lin" +.. pAllSemicList (longestOfSome (pJ pVBind) ... kw "=" +..  pTerm 
                             *** (\ (f:xx, t) -> DefLin f xx t))
 +||
  kw "lindef" +.. pAllSemicList (longestOfSome (pJ pVBind) ... kw "=" +..  pTerm 
                             *** (\ (f:xx, t) -> DefDefault f xx t))
 +||
  kw "var" +.. pAllSemicList (pVIdent ... pColons +.. someSep (kw ",") pVIdent
                             *** (\ (x,a) -> DefVar x a))
 +||
  kw "tokenizer" +.. pVIdent ..+ semi
                             *** (:[]) . DefTokenizer
--{-
 +||
  kw "pattern" +.. pAllSemicList (longestOfSome (pJ pVBind) ... kw "=" +..  pLinP 
                             *** (\ (f:xx, t) -> DefLin f xx t))
    where 
      pLinP = someSep (seps "++") (pJ (pTok +||
                                       (gParseToks *** mkToks) +|| 
                                       pVIdent *** linOfArg 0))
                                *** (\ts -> mkRecord linLabel [foldr1 Concat ts])
               +||
              seps "[]" <<< Tok zeroTok
--}

defLType c t = DefOper c TypeType t

pDefMacro :: ParseToken a => GParser [(Def,LDef a)]
pDefMacro =
  kw "category" +.. 
  pAllSemicList (pVIdent .... pContext ... kw "-" +.. pTerm 
                  *** (\ (cat,(cont,tr)) -> (DefCat cat cont, DefLintype cat tr)))
 +||
  kw "rule" +.. 
  pAllSemicList (pVIdent ... pColons +.. pTerm ... kw "-" +.. pTerm
                  *** (\ (f,(typ,tr)) -> (DefFun f typ, DefLin f (vrs typ) tr)))
   where vrs = varsOfType
--}
pTerm :: ParseToken a => GParser (Term a)
--pTerm :: GParser (Term Str)
pTerm =
  kw "\\" +.. pVarList ... seps "->" +.. pJ pTerm *** (\ (xx,b) -> mkAbs xx b)
 +||
  pDecl ... seps "->" +.. pJ pTerm *** (\ (cont,b) -> mkProd (cont,b,[]))
 +|| 
  pTuple RecType pTerm ... pTableArrow +.. pTerm *** uncurry Table
 +||
  kw "let" +.. pCurlyList pLocalDef ... kw "in" +.. pTerm *** uncurry Let
 +||
  kw "case" +.. pTerm ... kw "of" +.. pCurlyList pCase *** 
                                         (\ (t,cc) -> Select (Cases cc) [t])  
 +||
  pTerm05 ... 
    (seps "++" +.. pTerm *** flip Concat
      +||
     kw "+"  +.. pTerm *** flip Glue
      +||
     seps "->"  +.. pTerm *** (\valty -> \argty -> Prod wildId argty valty)
      +||
     seps "=>"  +.. pTerm *** (\valty -> \argty -> Table [argty] valty)
      +||
     kw "*"   +.. pCurly (pLabel ... pColons +.. pTerm) *** flip UpdRecType
      +|| 
     seps "**"  +.. pCurly (pLabel ... kw "="  +.. pTerm) *** flip UpdRecord
      +||
     succeed id)
      *** uncurry (flip ($))

pTerm05 :: ParseToken a => GParser (Term a)
--pTerm05 :: GParser (Term Str)
pTerm05 = 
  pTerm1 .>. (\ f -> 
  (longestOfMany (kw "!" +.. pJ pTerm1) *** flip mkSelects
      |||
   (kw "!" +.. pTuple Record pTerm) *** flip Select) .>. (\tt ->
  succeed (tt f)))

pTerm1 :: ParseToken a => GParser (Term a)
pTerm1 =
  longestOfSome (pJ pTerm2) *** (\ (x:xs) -> mkApp x xs) ---
 +||
  kw "Lin" +.. pTerm2 *** LiT
 +||
  jLay "table" +.. pCurlyList pCase *** Cases
 +||
  jLay "strs" +.. pCurlyList pTerm *** Strs
 +||
  jLay "pre" +.. pAltern *** Alts DPrefix
 +||
  jLay "post" +.. pAltern *** Alts DPostfix

--}
pTerm2 :: ParseToken a => GParser (Term a)
pTerm2 =
  pTerm3 ... 
    (longestOfMany (kw "." +.. pLabel) *** (\g t -> foldl Project t g)
       |||
     pCurlyList (pVIdent ... seps ":=" +.. pTerm) *** (\g t -> Closure g t)
    )                              -- added parser for Closure: AR 27/2/2001
     *** (\ (t,f) -> f t)

pTerm3 :: ParseToken a => GParser (Term a)
--pTerm3 :: GParser (Term Str)
pTerm3 =
  pParenth pTerm 
 +|| 
  pVIdent *** Var -- context-free parsing treats all ids as variables
 +||
  pLiteral *** uncurry Literal
 +||
  (kw "Strs" <<< TypeStrs +|| kw "Str" <<< TypeStr)
 +||
  kw "Type" <<< TypeType
 +||
  pMetaSymb *** Meta
 +||
  kw "<" +.. pTerm ... kw ":" +.. pTerm ..+ kw ">" *** uncurry Typed ---
 +|| 
  pCurly (longestOfMany (pJ pTerm2) ... pTableArrow +.. pTerm) *** uncurry Table
 +|| 
  pCurlyList pLabellings *** RecType . concat
 +|| 
  pCurlyList pAssigns *** Record . concat
 +||
  pTok
 +||
  gParseToks *** mkToks
 +||
  seps "[]" <<< Tok zeroTok

pLabellings, pAssigns :: ParseToken a => GParser [(Label, Term a)]
pLabellings = pLabelList ... pColons +.. pTerm *** (\ (xs,y) -> [(x,y) | x <- xs])
pAssigns = pLabelList ... jL "=" +.. pTerm *** (\ (xs,y) -> [(x,y) | x <- xs])
pLabelList = someSep (kw ",") pLabel


mkToks ss = if null ss then Tok zeroTok else foldr1 Concat (map Tok ss)

pTok :: ParseToken a => GParser (Term a)
pTok = gParseTok *** Tok

pContext :: ParseToken a => GParser (Context a)
pContext = longestOfMany (pJ (pDecl +|| pTerm3 *** (:[]) . mkDecl)) *** concat

pDecl :: ParseToken a => GParser [Decl a]
pDecl = 
  pParenth (pJ (pVarList ... pColons +.. pTerm)) 
    *** (\ (xx,t) -> [(x,t) | x <- xx])

pPatt :: GParser Patt
pPatt = 
  pVIdent ... longestOfSome (pJ pPatt2) *** uncurry PattCons
 +||
  pPatt2 

pPatt2 :: GParser Patt
pPatt2 =
  pParenth pPatt
 +||
  pCurly pVIdent *** flip PattCons [] --- will become obsolete thanks to new Rename
 +||
  pCurlyList (pLabelList ... jL "=" +.. pPatt *** (\ (xs,y) -> [(x,y) | x <- xs]))
               *** PattRec . concat
 +||
  pVIdent *** PattVar
 +||
  pWildCard <<< wildPatt

pWildCard = kw "_" 
wildId = zIdent "_"
pVBind = pVIdent ||| pWildCard <<< wildId

pVarList = someSep (kw ",") pVBind

pCase :: ParseToken a => GParser (Case a)
pCase =
   (pJ pPatt *** (:[])) ... pTableArrow +.. pTerm
 +||
   pTuple PattRec pPatt2 ... pTableArrow +.. pTerm

-- mortal strike to tuples, at last!
pTuple r p = pArgList2 (pJ p) *** (\ts -> [r (tuple2record ts)])

pAltern ::  ParseToken a => GParser (Altern a)
pAltern = pCurly (pTerm ... semi +.. pSemicList pVariant) where
 pVariant = pTerm ... kw "/" +.. pTerm *** (\ (x,y) -> (y,x))

pLocalDef :: ParseToken a => GParser (LocalDef a)
pLocalDef = 
  pVIdent ... pColons +.. pTerm ... kw "=" +.. pTerm *** (\ (c,(a,b)) -> (c,a,b))

pTableArrow = seps "=>"

pVIdent :: GParser Ident
--pVIdent = pIdent |> (not . (`elem` reservedGFWords)) *** zIdent
pVIdent = tclass I *** zIdent
-- context-free parsing numbers all identifiers 0

pMetaSymb :: GParser MetaSymb
pMetaSymb =
 seps "[?" +.. pVIdent ... longestOfMany (kw "'") ..+ seps "?]" 
   *** (\ (c,l) -> MetaSymb (c,length l))

pParams :: ParseToken a => GParser [Param a]
pParams = someSep (kw "|") (pVIdent .... pContext)

pConstrs :: GParser [Ident]
pConstrs = someSep (kw "|") pVIdent +|| succeed []

pLabel :: GParser Label
--pLabel = pLetters ... (pIntc +|| succeed 0) *** Label -- no other digits in label !
pLabel = tclass I .>. lbl
  where lbl s = case span isAlpha s of
		 (s1,[]) -> succeed (Label (s1,0))
	         (s1,s2) -> if all isDigit s2
			    then succeed (Label (s1,read s2))
			    else fails "<label>"

------- generic parsers

pCurly :: GParser a -> GParser a
pCurly p = kw "{" +.. p ..+ kw "}"

pSemicList, pCurlyList :: GParser a -> GParser [a]
pSemicList = someSep semi
pCurlyList = pCurly . pSemicList

pAllSemicList p = pSemicList p ..+ semi --- +|| succeed "")

jLay kw = jL kw --- +|| succeed kw 
-- can we omit keywords of curly lists?
-- perhaps table, but pre and post must be distinguished from each other!

------ new generic parsers
pParenth p = sep '(' +.. p ..+ sep ')'
pArgList2 p = pParenth (p ... jL "," +.. pCommaList p) *** uncurry (:) -- min.2 args
pCommaList = someSep (kw ",")
pJ = id
jL = kw
pFileName = strlit +|| pOldFileName
pOldFileName =
  some (tclass I +|| (sep '.' +|| sep '/' +|| sep '~') ***ptStr) *** concat

pLiteral = tclass I *** ((,) identString)
           +|| tclass N *** ((,) identInt)
p1 .... p2 = p1 ... p2

------ token level
semi = kw ";"
kw s = lit (K,s)
lit t = literal (PT noPos t)
strlit = tclass S *** (reverse . tail . reverse .tail)
tclass k =satisfy (\ (PT _ (k',_))->k'==k) *** ptStr
sep c = kw [c]
seps = foldr ((+..).sep) (succeed ())

--- to export (AR 29/1/2001)

pTrm :: String -> Trm
pTrm  = pWith pTerm (Var (zIdent "?"))

pTrm' :: String -> [Trm]
pTrm' s = either (const []) (:[]) $ newParseResults pTerm3 (glexpos s)

pMeta :: String -> MetaSymb
pMeta = pWith pMetaSymb (MetaSymb (zIdent "?",0))

pConstraints :: String -> [(Trm,Trm)]
pConstraints = pWith pCs [] where
  pCs = someSep (kw ",") (pTerm ... kw "=" +.. pTerm)

pWith p d s = case newParseResults p (glexpos s) of
   Right t -> t
   _ -> d
