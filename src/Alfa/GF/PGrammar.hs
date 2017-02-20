module PGrammar where

import Operations
import Parsers
import Tokens
import Grammar
import Macros
import Predefined

--- to test
import PrGrammar
testpTrm :: IO ()
testpTrm = do
 s <- getLine
 let t = pLTermStr s
 putStrLn (prt t)
 putStrLn (show t)
 testpTrm
--- end test

-- quick and dirty GF file parser AR 25/1/2000 -- 3/4
-- error reporting should be added...

-- 29/1/2001 trying to get rid of this - but still need it for some time
-- What really should be used is PGrammar2.

{-{- -- Bug workaround for nhc98-1.00 /TH
#ifdef __NHC__
#define DEFAULT -} default(Str) {-
#endif
-}-}
{-DEFAULT-}

pTrm :: String -> Trm
pTrm s = case parseResults pTerm s of
           t:_ -> t
           _ -> Var (zIdent "?")

pLTermStr :: String -> LTerm Str
pLTermStr s = case parseResults pTerm s of
                t:_ -> t
                _ -> Var (zIdent "?")

pGrammarStr :: String -> (Imports,Grammar Str)
pGrammarStr s = case parses pGrammar (remComments s) of
                  (t,_):_ -> t
                  _ -> ([],emptyGrammar)

-- grammar flags : imports
-- imports : list of grammar file names
type Imports = [String]
pImports = 
  jL "include" +.. pSemicList pFileName ..+ jL ";" ||| succeed []
  -- "import" is obsolete: this is more like an include operation

-- previously one, two, or more colons...
pColons = jL ":"

-- the main function

pGrammar :: Token a => Parser Char (Imports, Grammar a)
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

pDefinitions :: Token a => Parser Char [Either Def (LDef a)]
pDefinitions = pDef *** map Left ||| pLDef *** map Right ||| pDefMacro *** mkD where
 mkD dd = concat [[Left def, Right ldef] | (def,ldef) <- dd]

pDef :: Parser Char [Def]
pDef = 
  jL "cat" +.. pAllSemicList (pVIdent .... pContext *** uncurry DefCat)
 |||
  jL "fun" +.. pAllSemicList 
                  (pTList "," pVIdent ... pColons +.. pTerm *** 
                                (\ (fs,t) -> [DefFun f t | f <- fs])) 
        *** concat
 |||
  jL "type" +.. pAllSemicList (pVIdent ... pColons +.. pTerm *** uncurry DefType)
 |||
  jL "def" +.. pAllSemicList (pPatt ... jL "=" +..  pTerm *** uncurry DefDef)
 |||
  jL "data" +.. pAllSemicList (pVIdent ... jL "=" +..  pConstrs *** uncurry DefData)

pLDef :: Token a => Parser Char [LDef a]
pLDef =
  jL "param" +.. pAllSemicList (pVIdent ... jL "=" +.. pParams *** uncurry DefParam)
 |||
  jL "oper" +.. pAllSemicList (pVIdent ... pColons +.. pTerm ... jL "=" +.. pTerm 
                             *** (\ (c,(a,b)) -> DefOper c a b))
 |||
  jL "lintype" +.. pAllSemicList (pVIdent ... jL "=" +.. pTerm *** uncurry DefLType)
 |||
  jL "lincat" +.. pAllSemicList (pVIdent ... jL "="+.. pTerm *** uncurry DefLintype)
 |||
  jL "lindef" +.. pAllSemicList (longestOfSome (pJ pVBind) ... jL "=" +..  pTerm 
                             *** (\ (f:xx, t) -> DefDefault f xx t))
 |||
  jL "lin" +.. pAllSemicList (longestOfSome (pJ pVBind) ... jL "=" +..  pTerm 
                             *** (\ (f:xx, t) -> DefLin f xx t))
 |||
  jL "var" +.. pAllSemicList (pVIdent ... pColons +.. pTList "," pVIdent
                             *** (\ (x,a) -> DefVar x a))
 |||
  jL "tokenizer" +.. pVIdent ..+ jL ";"
                             *** (:[]) . DefTokenizer
 |||
  jL "pattern" +.. pAllSemicList (longestOfSome (pJ pVBind) ... jL "=" +..  pLinP 
                             *** (\ (f:xx, t) -> DefLin f xx t))
    where 
      pLinP = pTList "++" (pJ (parseTok *** Tok |||
                               parseToksTerm |||
                               pVIdent *** linOfArg 0))
                                *** (\ts -> mkRecord linLabel [foldr1 Concat ts])
               |||
              lits "[]" <<< Tok zeroTok
          
pDefMacro :: Token a => Parser Char [(Def,LDef a)]
pDefMacro =
  jL "category" +.. 
  pAllSemicList (pVIdent .... pContext ... jL "-" +.. pTerm 
                  *** (\ (cat,(cont,tr)) -> (DefCat cat cont, DefLintype cat tr)))
 |||
  jL "rule" +.. 
  pAllSemicList (pVIdent ... pColons +.. pTerm ... jL "-" +.. pTerm
                  *** (\ (f,(typ,tr)) -> (DefFun f typ, DefLin f (vrs typ) tr)))
   where vrs = varsOfType

pTerm :: Token a => Parser Char (Term a)
pTerm = 
  jL "\\" +.. pVarList ... jL "->" +.. pJ pTerm *** (\ (x,b) -> mkAbs x b)
 |||
  pDecl ... jL "->" +.. pJ pTerm *** (\ (cont,b) -> mkProd (cont,b,[]))
 ||| 
  pTuple RecType pTerm ... pTableArrow +.. pTerm *** uncurry Table
 |||
  jL "let" +.. pCurlyList pLocalDef ... jL "in" +.. pTerm *** uncurry Let
 |||
  jL "case" +.. pTerm ... jL "of" +..  pCurlyList pCase *** 
                                         (\ (t,cc) -> Select (Cases cc) [t])
 |||
  pTerm05 ... 
    (jL "++" +.. pTerm *** flip Concat
      |||
     jL "+"  +.. pTerm *** flip Glue
      |||
     jL "->"  +.. pTerm *** (\valty -> \argty -> Prod wildId argty valty)
      |||
     jL "=>"  +.. pTerm *** (\valty -> \argty -> Table [argty] valty)
      |||
     jL "*"   +.. pCurly (pLabel ... pColons +.. pTerm) *** flip UpdRecType
      ||| 
     jL "**"  +.. pCurly (pLabel ... jL "="  +.. pTerm) *** flip UpdRecord
      |||
     succeed id)
      *** uncurry (flip ($))

pTerm05 :: Token a => Parser Char (Term a)
pTerm05 = 
  pTerm1 .>. (\ f -> 
  (longestOfMany (jL "!" +.. pJ pTerm1) *** flip mkSelects
      |||
   (jL "!" +.. pTuple Record pTerm) *** flip Select) .>. (\tt ->
  succeed (tt f)))

pTerm1 :: Token a => Parser Char (Term a)
pTerm1 = 
  longestOfSome (pJ pTerm2) *** (\ (x:xs) -> mkApp x xs) ---
 |||
  jL "Lin" +.. pTerm2 *** LiT
 |||
  jLay "table" +.. pCurlyList pCase *** Cases
 |||
  jLay "strs" +.. pCurlyList pTerm *** Strs
 |||
  jLay "pre" +.. pAltern *** Alts DPrefix
 |||
  jLay "post" +.. pAltern *** Alts DPostfix

pTerm2 :: Token a => Parser Char (Term a)
pTerm2 =
  pTerm3 ... longestOfMany (lits "." +.. pLabel) *** uncurry (foldl Project)

pTerm3 :: Token a => Parser Char (Term a)
pTerm3 = 
  pParenth pTerm 
 ||| 
  pVIdent *** Var -- context-free parsing treats all ids as variables
 |||
  pLiteral *** uncurry Literal
 |||
  (lits "Strs" <<< TypeStrs +|| lits "Str" <<< TypeStr)
 |||
  lits "Type" <<< TypeType
 |||
  pMetaSymb *** Meta
 |||
  jL "<" +.. pTerm ... jL ":" +.. pTerm ..+ jL ">" *** uncurry Typed ---
 ||| 
  pCurly (longestOfMany (pJ pTerm2) ... pTableArrow +.. pTerm) *** uncurry Table
 ||| 
  pCurlyList pLabellings *** RecType . concat
 ||| 
  pCurlyList pAssigns *** Record . concat
 |||
  parseTok *** Tok
 |||
  parseToksTerm
 |||
  lits "[]" <<< Tok zeroTok

pLabellings, pAssigns :: Token a => Parser Char [(Label, Term a)]
pLabellings = pLabelList ... pColons +.. pTerm *** (\ (xs,y) -> [(x,y) | x <- xs])
pAssigns = pLabelList ... jL "=" +.. pTerm *** (\ (xs,y) -> [(x,y) | x <- xs])
pLabelList = pTList "," pLabel

parseToksTerm :: Token a => Parser Char (Term a)
parseToksTerm =
  parseToks .>. 
    (\ss -> succeed (if null ss then Tok zeroTok else foldr1 Concat (map Tok ss)))

pContext :: Token a => Parser Char (Context a)
pContext = longestOfMany (pJ (pDecl ||| pTerm3 *** (:[]) . mkDecl)) *** concat

pDecl :: Token a => Parser Char [Decl a]
pDecl = 
  pParenth (pJ (pVarList ... pColons +.. pTerm)) 
    *** (\ (xx,t) -> [(x,t) | x <- xx])

pPatt :: Parser Char Patt
pPatt = 
  pVIdent ... longestOfSome (pJ pPatt2) *** uncurry PattCons
 |||
  pPatt2 

pPatt2 :: Parser Char Patt
pPatt2 =
  pParenth pPatt
 |||
  pCurly pVIdent *** flip PattCons [] --- will become obsolete thanks to new Rename
 |||
  pCurlyList (pLabelList ... jL "=" +.. pPatt *** (\ (xs,y) -> [(x,y) | x <- xs]))
               *** PattRec . concat
 |||
  pVIdent *** PattVar
 |||
  pWildCard  <<< wildPatt

pWildCard = lits "_"
wildId = zIdent "_"
pVBind = pVIdent ||| pWildCard <<< wildId

pVarList = pTList "," pVBind

pCase :: Token a => Parser Char (Case a)
pCase = 
   (pJ pPatt *** (:[])) ... pTableArrow +.. pTerm
 |||
   pTuple PattRec pPatt2 ... pTableArrow +.. pTerm

-- mortal strike to tuples, at last!
pTuple r p = pArgList2 (pJ p) *** (\ts -> [r (tuple2record ts)])

pAltern ::  Token a => Parser Char (Altern a)
pAltern = pCurly (pTerm ... jL ";" +.. pSemicList pVariant) where
 pVariant = pTerm ... jL "/" +.. pTerm *** (\ (x,y) -> (y,x))

pLocalDef :: Token a => Parser Char (LocalDef a)
pLocalDef = 
  pVIdent ... pColons +.. pTerm ... jL "=" +.. pTerm *** (\ (c,(a,b)) -> (c,a,b))

pTableArrow = jL "=>"

pVIdent :: Parser Char Ident
pVIdent = pIdent |> (not . (`elem` reservedGFWords)) *** zIdent
-- context-free parsing numbers all identifiers 0

pMetaSymb :: Parser Char MetaSymb
pMetaSymb =
 jL "[?" +.. pVIdent ... longestOfMany (lits "'") ..+ jL "?]" 
   *** (\ (c,l) -> MetaSymb (c,length l))

pParams :: Token a => Parser Char [Param a]
pParams = pTList "|" (pVIdent .... pContext)

pConstrs :: Parser Char [Ident]
pConstrs = pTList "|" pVIdent +|| succeed []

pLabel :: Parser Char Label
pLabel = pLetters ... (pIntc ||| succeed 0) *** Label -- no other digits in label !

reservedGFWords = 
  ["Lin", "Str", "Strs","Type",
   "cat","category","def","fun","in","include","let","lin","lincat","lintype","of",
   "oper","param","pattern","post","pre","rule","strs","table","tokenizer","type",
   "var","case", "lindef",
   "grammar", "concrete", "abstract", "sig", "struct", "data" -- not actual
  ]

-- comment removal : line tails prefixed by -- as well as chunks in {- ... -}
remComments :: String -> String
remComments s = 
  case s of
    '"':s2 -> '"':pass remComments s2 -- comment marks in quotes are not removed!
    '{':'-':cs -> readNested cs
    '-':'-':cs -> readTail cs
    c:cs -> c : remComments cs
    [] -> []
   where
     readNested t = 
       case t of
         '"':s2 -> '"':pass readNested s2
         '-':'}':cs -> remComments cs
         _:cs -> readNested cs
         [] -> []
     readTail t = 
       case t of
         '\n':cs -> '\n':remComments cs
         _:cs -> readTail cs
         [] -> []
     pass f t = 
       case t of
         '"':s2 -> '"': f s2
         c:s2 -> c:pass f s2
         _ -> t


------- generic parsers

pCurly :: Parser Char a -> Parser Char a
pCurly p = jL "{" +.. p ..+ jL "}"

pSemicList, pCurlyList :: Parser Char a -> Parser Char [a]
pSemicList = pTList ";"
pCurlyList = pCurly . pSemicList

pAllSemicList p = pSemicList p ..+ (jL ";" ) --- ||| succeed "")

jLay kw = jL kw --- ||| succeed kw 
-- can we omit keywords of curly lists?
-- perhaps table, but pre and post must be distinguished from each other!
