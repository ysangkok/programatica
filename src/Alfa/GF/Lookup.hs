module Lookup where
import Operations
import Tokens
import Grammar
import PrGrammar
import Macros
import SymbolTable
import Rename

-- lookup functions not involving computation of terms or types. AR 25/1/2000 --31/3

--- this is lookup in binary search trees; we ignore the numbering of constants

lookupAbstract :: Ident -> AbstractST -> Err IdentInfo
lookupAbstract i = (lookupTreeEq prt eqStrIdent i) . fst

lookupConcrete :: Token a => Ident -> ConcreteST a -> Err (IdentLInfo a)
lookupConcrete i = (lookupTreeEq prt eqStrIdent i) . fst

lookupCat :: Ident -> AbstractST -> Err Cont
lookupCat c st = do
  info <- lookupAbstract c st
  (case info of
     IdentCat cont -> return cont
     _ -> prtBad "no type found for constant" c)

lookupType :: Ident -> AbstractST -> Err Type
lookupType c st = do
  info <- lookupAbstract c st
  (case info of
     IdentType typ -> return typ
     _ -> prtBad "no type found for constant" c)

lookupFun :: Ident -> AbstractST -> Err Type
lookupFun c st = do
  info <- lookupAbstract c st
  (case info of
     IdentFun typ  -> return typ
     _ -> prtBad "no type found for constant" c)

defsOfFun :: Ident -> AbstractST -> [(Patt,Trm)]
defsOfFun f st = 
  let infos = tree2list (fst st)    --- not efficient to do every time
      atf = zIdent ("@" ++ symid f) --- encoding in SymbolTable.mkIdentInfo
  in [pt | (f', IdentDef pts) <- infos, eqStrIdent atf f', pt <- pts]

uniqueDefOfFun :: Ident -> AbstractST -> Err Trm
uniqueDefOfFun f st = case defsOfFun f st of
  [(patt,trm)] -> do
    xx <- pvars patt
    return $ mkAbs xx trm
  _ -> prtBad "no one-clause definition of" f
 where
   pvars (PattCons _ ps) = return [x | PattVar x <- ps]
   pvars _ = Bad "no pvars"

funsForCat :: Cat -> AbstractST -> [(Fun,Type)]
funsForCat c st =
  let infos = tree2list (fst st)  --- not efficient to do every time
  in [(f,typ) | (f, IdentFun typ) <- infos, 
                let Ok c' = valCat typ, eqStrIdent c c'] --- type equality ?

allCats :: AbstractST -> [Cat]
allCats st = let infos = tree2list (fst st) in [c | (c, IdentCat _) <- infos]

allDefs :: AbstractST -> [(Patt,Trm)]
allDefs st = let infos = tree2list (fst st) in concat [d | (_, IdentDef d) <- infos]

lookupLinTypeOfCat :: Token a => Cat -> ConcreteST a -> Err (LType a)
lookupLinTypeOfCat c st = do
  info <- case lookupConcrete c st of
            Ok i -> return i
            _    -> return $ IdentLintype defaultLinType
  (case info of
     IdentLintype t -> return t
     _ -> Bad ("lintype expected for category" +++ prt c))

lookupLinTypeMacro :: Token a => Ident -> ConcreteST a -> Err (LType a)
lookupLinTypeMacro c st = case lookupConcrete c st of
  Ok (IdentLType t) -> return t
  Ok (IdentOper ty tr) -> return tr --- prepare removing DefLType
  Ok (IdentTagType _) -> return (Cons c) --- does this belong here ?
  Ok _ -> prtBad "type macro expected instead of" c
  _  -> prtBad "looking for lintype macro, failed to find constant" c

lookupLinType :: Token a => Ident -> ConcreteST a -> Err (LType a)
lookupLinType c st = do
  info <- lookupConcrete c st
  (case info of
     IdentTag pt -> return pt
     IdentOper t _ -> return t
     IdentTagType _ -> return TypeType
     IdentLType _ -> return TypeType
     _ -> prtBad "no lin type found for constant" c)

lookupLin :: Token a => Ident -> ConcreteST a -> Err (LTerm a)
lookupLin c st = do
  info <- lookupConcrete c st
  (case info of
     IdentTagType _ -> return (Cons c)
     IdentTag _ -> return (Cons c)
     IdentOper _ t -> return t
     IdentLin xx t -> return (mkAbs xx t)
     _ -> prtBad "no linterm found for constant" c)

lookupDefaultOfCat :: Token a => Cat -> ConcreteST a -> Err ([Ident], LTerm a)
lookupDefaultOfCat c st = case lookupConcrete (mkAdHocIdent c) st of
  Ok (IdentLin xx t) -> return (xx, t)
  _ -> prtBad "no linearization default found for" c

lookupLinearization :: Token a => Fun -> ConcreteST a -> Err ([Ident], LTerm a)
lookupLinearization f st = case lookupConcrete f st of
  Ok (IdentLin xx t) -> return (xx, t)
  _ -> prtBad "no linearization rule found for" f

lookupTokenizer :: Token a => ConcreteST a -> Ident
lookupTokenizer st = case lookupConcrete (zIdent "tokenizer") st of
  Ok (IdentTokenizer t) -> t
  _ -> zIdent "mini"

-- find all possible tags of a tag type

lookupTags :: Token a => LType a -> ConcreteST a -> Err [LTerm a]
lookupTags (Cons typ) st =
 case lookupConcrete typ st of
   Ok (IdentTagType params) -> do tt <- mapM getTags params
                                  return (concat tt)
   Ok _  -> prtBad "no tag type" typ
   Bad s -> prtBad (s ++ "when looking for tags of type") typ
  where 
    getTags (t,[])   = return [Cons t]
    getTags (t,cont) = do
      tt <- mapM ((flip lookupTags st) . snd) cont
      return (map (mkApp (Cons t)) (combinations tt))
lookupTags (RecType r) st = do
  let (ls,tys) = unzip r
  tss <- mapM (flip lookupTags st) tys
  return [Record (zip ls ts) | ts <- combinations tss]
lookupTags t st = prtBad "to find tags a tag type is expected instead of" t

lookupTagss :: Token a => [LType a] -> ConcreteST a -> Err [[LTerm a]]
lookupTagss typs st = mapM (flip lookupTags st) typs

allPatts :: Token a => [LType a] -> ConcreteST a -> Err [[Patt]]
allPatts typs st = do
  tt <- lookupTagss typs st
  mapM (mapM term2patt) tt

