module OldPGF where

import Operations
import OldGFParsers
import OldGF
import OldPredefined

-- modified from parser for GF grammar files, v. 0.54. AR 19/6/1998 -- 2/2/2000
-- Copyright (c) Aarne Ranta 1998--99, under GNU General Public License (see GPL)

-- ******************* the old PTypes

type GPEnv = (Grammar, [OIDent], [(OIDent, (Int,([(Symb,Type)],Cat,[Term])))])
emptyGPEnv = (emptyGrammar, [], [])

pTerm :: GPEnv -> Parser Char Term
pTerm env = 
 (pParenth (pTJList ")" "(" (pSymb env)) ||| succeed []) .>. (\xx ->
 pJ (pAtom env)                                          .>. (\a  ->
 pArgList (pTerm env)                                    .>. (\bb ->
 succeed (termForm2Term(xx,a,bb)))))

pType :: GPEnv -> Parser Char Type
pType env =
 (pParenth (pTJList ")" "(" (pDecl env)) ||| succeed []) .>. (\xx ->
 pJ (pCat env)                                           .>. (\a  ->
 pArgList (pTerm env)                                    .>. (\bb ->
 succeed (typeForm2Type(xx,a,bb)))))

pAtom :: GPEnv -> Parser Char Term
pAtom env = pFun env *** Cons ||| pSymb env *** Var ||| pMeta env *** Meta
            ||| pPredefFun *** Predef ---pd

pContext :: GPEnv -> Parser Char Context
pContext env = pParenth (pTList "," (pDecl env)) ||| succeed []

pDecl :: GPEnv -> Parser Char (Symb,Type)
pDecl env = pSymb env ... jL ":" +.. pType env

pDefinition :: GPEnv -> Parser Char Definition
pDefinition env = 
  pPatt env                        .>. (\pt ->
  jL "="                           .>. (\_  -> 
  (pTerm env                       .>. (\tr ->
  (jL ":" ..+ pType env     ||| succeed "")  .>. (\_ ->
  (pJ (pContext env) <<< "" ||| succeed "")  .>. (\_ ->
   succeed (DfRec pt tr)))))
    |||
  (literal '{'                            .>. (\_  -> 
   (pTList "," pIdent ||| succeed [])     .>. (\cc ->
   literal '}'                            .>. (\_  ->
  (jL ":" ..+ pType env     ||| succeed "")  .>. (\_ ->
  (pJ (pContext env) <<< "" ||| succeed "")  .>. (\_ ->
   succeed (DfData pt cc)))))))))

pPatt :: GPEnv -> Parser Char Patt
pPatt env = 
  pFun env ... pArgList (pPatt env) *** (\ (c,l) -> FPatt c l)
 |||
  pCat env ... pArgList (pPatt env) *** (\ (c,l) -> CPatt c l)
 |||
  pSymb env *** APatt
 |||
  lits "_"  *** APatt . Symb

pMeta :: GPEnv -> Parser Char (Cat,OIDent)
pMeta env = 
  literal '[' +..
  pCat env ...
   (longestOfMany (literal '\'') ... (literal '.' +.. pIntc *** (\n -> '.': show n) 
                                       +|| 
                                      succeed "")
      *** (\ (p,s) -> p ++ s)
     |||
   pJunk +.. longestOfSome (satisfy (/=']'))
     +||
   succeed [])
  ..+ literal ']'

-- identifiers

pCat :: GPEnv -> Parser Char Cat
pCat env@((_,(cats,_,_,_)),_,_) = 
  pLongestIdent (map fst cats ++ map fst predefCats) *** Cat ---pd

pFun :: GPEnv -> Parser Char Fun
pFun env@((_,(_,_,rules,_)),_,_) = pLongestIdent (map fst rules) *** Fun

pSymb :: GPEnv -> Parser Char Symb
pSymb env@((_,(_,_,rules,_)),_,_) = pVarIdent (map fst rules) *** Symb

pPredefFun :: Parser Char (OIDent,String)               ---pd
pPredefFun =                                           ---pd
 pLongestIdent (map fst predefRules) ... jL "{" +.. ---pd
 longestOfMany (satisfy (/= '}'))    ..+ jL "}"     ---pd

-- ******************* the old PGrammar

pGrammar :: GPEnv -> Parser Char Grammar
pGrammar env@(gr@((tok,pars,ops),(cats,vars,rules,defs)),tvars,avars) =
 pJunk +..
 (jL "Tokenizer"  +.. pTokenizer ||| succeed tok)                    .>. (\t00 ->
 let t0 = composeTokenizers tok t00 in
  optField "Parametres"  
   (pParametres (((t0,pars,ops),(cats,vars,rules,defs)),tvars,avars)) .>. (\p ->
 let pars' = pars ++ p in 
  optField "Operations"  
   (pOperations (((t0,pars',ops),(cats,vars,rules,defs)),tvars,avars)) .>. (\oo ->
    let ops' = ops ++ oo in 
  optField "Categories"  
   (pCategories (((t0,pars',ops'),(cats,vars,rules,defs)),tvars,avars)) .>. (\c ->
  let cats' = cats ++ c
      t     = composeTokenizers (Tokenizer "mini" [ct | (ct,_) <- c]) t0 in
   optField "Variables"   
    (pVariables (((t,pars',ops'),(cats',vars,rules,defs)),tvars,avars)) .>. (\v -> 
   let vars' = vars ++ v in 
    optField "Operations"  
     (pOperations (((t,pars',ops'),(cats',vars',rules,defs)),tvars,avars)) .>. (\o ->
    let ops'' = ops' ++ o in 
     optField "Rules" 
      (pRules (((t,pars',ops''),(cats',vars',rules,defs)),tvars,avars))   .>. (\r ->
     let rules' = rules ++ r in 
      optField "Definitions" 
       (pDefinitions (((t,pars',ops''),(cats',vars',rules',defs)),tvars,avars)) .>.(\d ->
      let defs' = defs ++ d in
        succeed ((t, pars', ops''),(cats', vars', rules', defs'))))))))))
--- operations can occur both in the new (their logical) place or old place

-- parsers for fields

pTokenizer :: Parser Char Tokenizer
pTokenizer = 
  pIdent .... 
  (optlitt "Tokens" +.. pJ (pCommaList pQuotedString) ||| succeed []) ..+ jL ";"
   *** (\ (i,k) -> Tokenizer i k)

pParametres  :: GPEnv -> Parser Char [(OIDent,[(Tag,[TagType])])]
pParametres env = pManyDefs pParDef updpars env where
 updpars pdef env@(((tok,pars,ops),(cats,vars,rules,defs)),tvars,avars) =
   (((tok,pars ++ [pdef],ops),(cats,vars,rules,defs)),tvars,avars)
 pParDef env = pIdent .... (pArgList ((pIdent *** Tag) ... pArgList (pTagType env)))

pCategories :: GPEnv -> Parser Char [(OIDent,CatDef)]
pCategories env = pManyDefs pCatDef updcats env where
 updcats cdef env@(((tok,pars,ops),(cats,vars,rules,defs)),tvars,avars) =
   (((tok,pars,ops),(cats ++ [cdef],vars,rules,defs)),tvars,avars)

pVariables  :: GPEnv -> Parser Char [VarSpec]
pVariables env = 
 pTList ";" (pJ (pCommaList pIdent) .... pArgList (pCat env))  ..+ jL ";"

pOperations   :: GPEnv -> Parser Char [(OIDent,OpDef)]
pOperations env = pManyDefs pOpDef updops env where
 updops odef env@(((tok,pars,ops),(cats,vars,rules,defs)),tvars,avars) =
   (((tok,pars,ops ++ [odef]),(cats,vars,rules,defs)),tvars,avars)

pRules :: GPEnv -> Parser Char [(OIDent,Rule)]
pRules env = pManyDefs pRule updrules env where
 updrules rule env@(((tok,pars,ops),(cats,vars,rules,defs)),tvars,avars) =
   (((tok,pars,ops),(cats,vars,rules ++ [rule],defs)),tvars,avars)

pDefinitions  :: GPEnv -> Parser Char [Definition]
pDefinitions env = pTList ";" (pDefinition env)  ..+ jL ";"

-- parsers for smaller items

pCatDef  :: GPEnv -> Parser Char (OIDent,CatDef)
pCatDef env = 
 pIdent  .... (pArgList (pCat env) *** mkContext ||| pContext env) ... 
 ((jL "-" +.. pOptCommaList (pTagType env) ...
  (jL "-" +.. pOptCommaList (pTagType env) ||| succeed []))
   |||
  succeed ([],[])) ...
 (jL "-" +.. pIntc ||| succeed 1)
  *** (\ (cat,(args,((pars,inhs),ari))) -> (cat,(args,pars,inhs,ari)))
   where
    mkContext cc = [(numSymb "x" n, Ground c []) | (n,c) <- zip [0..] cc]

pOpDef :: GPEnv -> Parser Char (OIDent,OpDef)
pOpDef env = 
 pIdent                            .>. (\op ->
 pJ (pArgList (pTagType env))      .>. (\tt ->
 jL "="                            .>. (\_  ->
 (pStrOp1 env *** DefStrOp tt    
     |||
  pFeatOp1 env ... jL ":" +.. pTagType env 
              *** (\ (f,ty) -> DefFeatOp tt ty f))     .>. (\def ->
 succeed (op,def)))))
   |||
 pIdent                                                .>. (\op ->
 (jL "[" +.. pTList "," (pNewLinDecl env) ..+ jL "]")  .>. (\cont -> 
 pArgList (pTagType env)            .>. (\tt ->
 jL "="                             .>. (\_ ->
 pStrOp1 env                        .>. (\strop ->
 succeed (op,DefNewOp cont tt strop))))))

pNewLinDecl :: GPEnv -> Parser Char (OIDent,NewLinType)
pNewLinDecl env = pLinVar ... jL ":" +.. pNewLinType env

pNewLinType :: GPEnv -> Parser Char NewLinType
pNewLinType env = (pArgList (pTagType env) ..+ jL "->" ||| succeed []) .... pIntc

pLinVar :: Parser Char OIDent
pLinVar = literal '#' +.. pIdent

pRule :: GPEnv -> Parser Char (OIDent,Rule)
pRule env = 
  pIdent                          .>. (\fun -> 
  ((jL "." +.. pCfEntry env)                    .>. (\cfent -> 
   succeed (fun,cfent)))
    |||
  ((jL ":" +.. pType env)                       .>. (\ty -> 
   (jL "-" +.. pEntry (penv env ty))            .>. (\ent ->
   succeed (fun,(ty,ent))))))
   where 
    penv (gr,tvars,_) ty = 
     (gr,tvars,[(x,(n,typeForm t)) | 
                ((x,t),n) <- zip [(x,t) | (Symb x,t) <- xx] [0..]])
                                             where (xx,_,_) = typeForm ty

pEntry :: GPEnv -> Parser Char Entry
pEntry env = pStrOp1 env ... (jL "-" +.. pFeatures env ||| succeed [])

pCfEntry :: GPEnv -> Parser Char (Type,Entry)
pCfEntry env =
 pCat env ... jL "->" +..  
 longestOfMany (pJ (pCat env *** Left +|| pCfString *** Right))
   *** mkCfE
  where
   mkCfE (c,x) = (mkType x c, (mkStrOp x, []))
   nSymb n     = numSymb "x" n
   mkType x c  = typeForm2Type
                   (zip (map nSymb [0..]) [Ground a [] | Left a <- x],c,[])
   mkStrOp x   = StrOpCase [([],[combStrs (mkStr 0 x)])]
   mkStr n x   = case x of []         -> []
                           Left _  :k -> StrArg  n 0 [] : mkStr (n+1) k 
                           Right s :k -> StrCons [s] [] : mkStr  n k
   pCfString   = longestOfSome (satisfy (\x -> not (elem x " \n\t;")))

pStr2 :: GPEnv -> Parser Char Str
pStr2 env@(gr@((tok,_,_),_),_,avars) =
  pQuotedString 
    *** (\s -> StrCons (words s) [])
 |||
  pLookup avars ... pEitherOrder pIndex (pArgList (pFeature env))
    *** (\ ((n,_),(i,ff)) -> StrArg n i ff)
 |||
  pStrOp2 env ... pEitherOrder pIndex 
                      ((jL "[" +.. pTList "," (pStr2 env) ..+ jL "]" ||| succeed [])
                        ... pArgList (pFeature env))
    *** (\ (op,(i,(ss,ff))) -> StrApp op ss ff i)
 |||
  pParenth (pStr1 env)
 |||
  pLinVar .... pArgList (pFeature env) *** uncurry StrNewArg
 |||
  pBound                                                .>. (\ (n,i) ->
  (literal '@' +.. pLookup avars *** fst ||| succeed n) .>. (\m ->
  (succeed (StrBound m i)))) 
    where pBound = pLookup (foldl (++) [] 
                              [[(x,(n,i)) | ((Symb x,_),i) <- zip xx [0..]] |
                                                     (_,(n, (xx,_,_))) <- avars])

pStr1 :: GPEnv -> Parser Char Str
pStr1 env@(gr@((tok,_,_),_),_,_) =
  (pQuotedString *** words)                         .>. (\s ->   
  jL ","                                            .>. (\_ ->
  pTList "," ((pQuotedString *** words) .... 
              pParenth (pTList "," pQuotedString)
               *** (\ (x,y) -> (y,x)))              .>. (\ss ->
  succeed (StrCons s ss))))
 |||
  pStr2 env                                         .>. (\s ->
  longestOfMany (pJ pJunct .... pStr2 env)          .>. (\ss ->
  succeed (combs s ss)))
 |||
  (jL "let" +.. pOpDef env ..+ jL "in")    .>. (\ locdef@(var,df) ->
  pStr1 (addOp locdef)                     .>. (\ str ->
  succeed (StrLet var df str)))
   where 
    combs s [] = s
    combs s ((b,s'):ss) = StrComb s b (combs s' ss)
    addOp locdef = 
     case env of 
       (((toks,pars,ops),rest),tv,av) -> (((toks,pars,locdef : ops),rest),tv,av)

pJunct :: Parser Char Junct
pJunct = literal '+' <<< True ||| succeed False

pStrs :: GPEnv -> Parser Char [Str]
pStrs env =
  pStr1 env *** (\s -> [s])
 |||
  pParenth (pTList "," (pStr1 env))

pStrOp2 :: GPEnv -> Parser Char StrOp
pStrOp2 env = 
  pStrOpId env *** StrOpId
 |||
  pParenth (pStrOp1 env)

pStrOp1 :: GPEnv -> Parser Char StrOp
pStrOp1 env = 
  pStrs env *** (\s -> StrOpCase [([],s)])
 |||
  jL "case" +.. pCommaList (pOpOneCase env pStrs) *** StrOpCase
 |||
  pStrOp2 env

pFeatures :: GPEnv -> Parser Char [Feature]
pFeatures env = pTList "," (pFeature env)

pFeature :: GPEnv -> Parser Char Feature
pFeature env@(gr,tvars,avars) = 
  pTagExp env *** FeatAtom
 |||
  pFeatOp2 env ... pArgList (pFeature env) *** (\ (op,ff) -> FeatApp op ff)
 |||
  pTagType env ... pEitherOrder pIndex (pParenth (pLookup avars)) 
   *** (\ (tt,(i,(n,(_,cat,_)))) -> FeatArg n (inhOf tt i cat))
    where 
     inhOf tt i cat   = case lookupCat gr cat of (_,_,pp,_) -> tkInh tt i pp
     tkInh tt n []    = 0   --- just to have a default ; made harmless by check gr
     tkInh tt 0 (a:k) = if a==tt then 0 else (tkInh tt 0 k) + 1
     tkInh tt n k     = tkInh tt (n-1) (tail (dropWhile (/=tt) k))

pFeatOp2 :: GPEnv -> Parser Char FeatOp
pFeatOp2 env =
  pFeatOpId env *** FeatOpId
 |||
  pParenth (pFeatOp1 env)

pFeatOp1 :: GPEnv -> Parser Char FeatOp
pFeatOp1 env =
  pFeature env *** (\f -> FeatOpCase [([],f)])
 |||
  jL "case" +.. pCommaList (pOpOneCase env pFeature) *** FeatOpCase
 |||
  pFeatOp2 env

pTagExp :: GPEnv -> Parser Char TagExp
pTagExp env@(((_,pars,ops),_),_,_) =
  pVarIdent (tags ++ map fst ops) *** TagVar 
 |||
  pTag env ... pArgList (pTagExp env) *** 
    (\ (t,xx) -> case xx of 
                   [] -> TagCons t
                   _ -> NewTagExp t xx)
  where tags = [t | (_,tt) <- pars, (Tag t,_) <- tt]

-- identifiers

pTagType :: GPEnv -> Parser Char TagType
pTagType env@(((_,pars,_),_),_,_) = pLongestIdent (map fst pars) *** TagType

pTag :: GPEnv -> Parser Char Tag
pTag env@(((_,pars,_),_),_,_) = pLongestIdent tags *** Tag where
 tags = [t | (_,tt) <- pars, (Tag t,_) <- tt]

pTagVar :: GPEnv -> Parser Char OIDent
pTagVar env@(((_,pars,_),_),_,_) = pVarIdent tags where
 tags = [t | (_,tt) <- pars, (Tag t,_) <- tt]

pFeatOpId :: GPEnv -> Parser Char OIDent
pFeatOpId env@(((_,_,ops),_),_,_) = pLongestIdent featops where
 featops = [op | (op,DefFeatOp _ _ _) <- ops]

pStrOpId :: GPEnv -> Parser Char OIDent
pStrOpId env@(((_,_,ops),_),_,_) = pLongestIdent strops where
 strops = [op | (op,DefStrOp _ _) <- ops] ++ [op | (op,DefNewOp _ _ _) <- ops]

-- auxiliary functions

-- parser for a projection index
pIndex :: Parser Char Int
pIndex = literal '.' +.. pIntc ||| succeed 0 --- previously pIntC *** (-1). 2/2/2000

-- parser for a branch in a definition by cases
pOpOneCase ::  GPEnv -> (GPEnv -> Parser Char a) -> Parser Char ([TagExp],a)
pOpOneCase env@(gr,tvars,avars) parser =
 pArgList (pTagExp env ||| lits "_" *** TagVar)  .>. (\t ->
 (jL "->" +.. parser (updtags t))                .>. (\s ->
 succeed (t,s)))
  where updtags t = (gr, tvars ++ [x | TagVar x <- t, x /= "_"], avars)

-- parser for a list of progressively dependent definitions
pManyDefs :: 
  (GPEnv -> Parser Char a) -> (a -> GPEnv -> GPEnv) -> GPEnv -> Parser Char [a]
pManyDefs pdef upd env =
 pdef env                       .>. (\x ->
 jL ";"                         .>. (\_ ->
 (pManyDefs pdef upd (upd x env) 
   |||
  succeed [])                   .>. (\xx ->
 succeed (x:xx))))

combStrs :: [Str] -> Str
combStrs ss =
 case ss of
  []   -> StrCons [] []
  s:[] -> s
  s:t  -> StrComb s False (combStrs t)

-- comment removal for grammar files

remove_comments [] = []
remove_comments ('"':s)     = '"':process_quote s
remove_comments ('(':'*':s) = process_comment s
remove_comments ('*':'*':s) = dropWhile (/= '\n') (remove_comments s)
remove_comments (c:s) = c : remove_comments s
process_comment [] = []
process_comment ('*':')':s) = remove_comments s
process_comment (c:s) = process_comment s
process_quote   ('\\':'\\':s) = '\\':'\\':process_quote s
process_quote   ('\\':'"':s) = '\\':'"':process_quote s
process_quote   ('"':     s) = '"':remove_comments s
process_quote   (c:s) = c:process_quote s
process_quote   [] = []

-- import removal

remove_imports :: String -> ([String],String)
remove_imports s =
 case s of 
   c:k | c `elem` " \t\n"    -> remove_imports k
   'I':'m':'p':'o':'r':'t':k -> (dImps s1,tail s2) where (s1,s2) = span (/= ';') s
   _                         -> ([], s)

-- deterministic parser for imports ; only read by GF command interpreter

dImps :: String -> [String]
dImps s =
--- case parses pImps s of (ff,_):_ -> ff
 case        pImps s of (ff,_):_ -> ff
                        _        -> []
  where pImps = pJ (jL "Import" +.. pTList "," pFileName)

-- ************** from old IOGrammar

getGrammarFromFile = grammarFromFile emptyGrammar

grammarFromFiles :: Grammar -> [String] -> IO (Grammar,String)
grammarFromFiles eg []  = return (eg,"") 
grammarFromFiles eg (s:ss) =
  do
  (eg1,ms1) <- grammarFromFile eg s 
  (eg2,ms2) <- grammarFromFiles eg1 ss
  return (eg2, ms1 ++ "\n" ++ ms2)

grammarFromFile :: Grammar -> String -> IO (Grammar,String)
grammarFromFile eg f =
  do
  s <- readFileIf f
  let (imps,s2) = remove_imports (remove_comments s) in
   (
    case imps of
      [] -> 
       grammarFromString eg f s2
      _ -> 
       do
       (g0, m0) <- grammarFromFiles  eg imps
       (g1, m1) <- grammarFromString g0 f s2
       return (g1, m0 ++++ m1)
   )

grammarFromString :: Grammar -> String -> String -> IO (Grammar,String)
grammarFromString eg f s =
       let
         pa = (pGrammar (eg,[],[])) s
         gr = case pa of (x,_):_ -> x
                         _       -> eg
         ms = case pa of (x,[]):_ -> "Grammar "++f++" accepted completely."
                         (x,r) :_ -> "Grammar "++f++" accepted only till"+++ 
                                     take 177 r
                         _        -> "No new grammar added." in
        return (gr,ms)
