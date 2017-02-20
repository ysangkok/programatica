{-| 
  Agda abstract syntax which is produced by the parser

  The name CSyntax originates from `Cayenne Syntax´, which served as a
  starting point for Agda. (Lennart says it actually meant `concrete syntax´.)
-}

module CSyntax(module CSyntax, pprId,module MetaVars) where
import BinParse(Fixity(..),prec)
import Error
import Position
import Id
import Literal
import MetaVars

data CProgram
        = CProgram [CModule]    
    --  | ErrProgram EMsg
        deriving (Eq, Ord)

data CModule
        = CModule Id [CArg] CPackageBody
        deriving (Eq, Ord)


type Comment = String

data CExpr =   
     CVar Id
   | CStar Position Int Int  -- ^ #0 = Set, #1 = Type ... (second Int unused)
   | Clam CArg CExpr
   | CUniv CArg CExpr
   | CArrow Bool CExpr CExpr
   | Clet [CLetDef] CExpr
   | CProduct Position [CSign]
   | CRecord [CProp] Position [CLetDef]
   | Copen CExpr COpenArgs CExpr
   | CSelect CExpr Id
   | CSum CSummands
   | CCon Id CType
   | CConS Id
   | Ccase CExpr CCaseArms
   | CApply CExpr [(Bool,CExpr)]
   | CBinOp CExpr Id CExpr
   | CMeta Position  Bool Visibility MetaVar -- first Bool indicates what is allowed
                                       -- in the metaexp, snd if should be automatically solvable or not
   | CClos CEnv CExpr         -- Only for printing
   | Ccomment Bool Comment CExpr -- True if comment is to the left
   | CPackageType   -- Just for printing
   | CIndSum [CArg] [CIndSummand]
   | Cif CExpr CExpr CExpr
   | CLit Position Literal
           --  ^ the telescope over which this is inductive
        deriving (Eq, Ord,Show)


type CEnv = [(Id,CExpr)]


cApply e [] = e
cApply (CApply e []) as = CApply e as
cApply e as = CApply e as

cVar v = CVar v

cLam :: [CArg] -> CExpr -> CExpr
cLam [] e = e
cLam (b:bs) e = Clam b (cLam bs e)


cUniv1 :: CArg -> CExpr -> CExpr
cUniv1 (CArg ((hidden,x):xs) a) b | isDummyId x = 
    CArrow hidden a (cUniv1 (CArg xs a) b)
cUniv1 (CArg [] a) b  = b 
cUniv1 cb b = CUniv cb b
 
cUniv :: [CArg] -> CExpr -> CExpr
cUniv cb b = foldr cUniv1 b cb



cSet :: Position -> CExpr
cSet pos = CStar pos 0 (-1)

cType :: Position -> CExpr
cType pos = CStar pos 1 (-1)

type CType = CExpr
type CArgs = [CArg]
type CSummand = (Id, CArgs)
type CSummands = [CSummand]
type CCaseArms = [(CPat, CExpr)]
type CIndSummand = (CSummand,[(Bool,CExpr)])
                         --  ^ substitution

data CProp 
        = Cprivate 
        | Cpublic 
        | Cabstract 
        | Cconcrete
        deriving (Eq, Ord, Show)

data COArg
        = COArgT [CProp] Id CType
        | COArg [CProp] Id
        | COArgAs [CProp] Id Id
        | COArgAsT [CProp] Id CType Id
        deriving (Eq, Ord,Show)

 
type COArgs = [COArg]
data COpenArgs
        = COpenArgs [COArg]
--      | COpenAll
        deriving (Eq, Ord,Show)


data CDef = CDef [CProp] CDefn | CDefComment Comment
        deriving (Eq, Ord,Show)

data CLetDef = CSimple CDef | CMutual [CDef] | CLetDefComment Comment
--             | CErrDef EMsg
         deriving (Ord,Eq,Show)

mapCLetDef :: (CDef -> CDef) -> CLetDef -> CLetDef
mapCLetDef f (CSimple d) = CSimple (f d)
mapCLetDef f (CMutual ds) = CMutual (map f ds)
mapCLetDef _ d = d


flattenCLet :: CLetDef -> [CDef]
flattenCLet (CSimple d) = [d]
flattenCLet (CMutual ds) = ds
flattenCLet _ = []

data CDefn
        = CValueT Id [CArg] CType CExpr
        | CValueS Id [CArg] CType CClause
--  | CValueP Id [CClause]
        | Ctype Id CArgs CType
        | Cnewtype Id CArgs CType CSummand
        | Cdata Id CArgs (Maybe CType) CSummands
        | CValue Id CExpr
        | CAxiom Id [CArg] CType 
        | CNative Id CType 
       --  | CPackage Id [CArg] [CProp] Position [CLetDef]
        | CPackage Id [CArg] CPackageBody
        | COpen CExpr COpenArgs
        | CClass CClassArg Bool [CSign] -- should maybe rather be a CDef? 
        | CInstance Id [CArg] CInstanceArg [CLetDef]

       --  | CDSign Id CType               -- Used only while type checking
         deriving (Eq, Ord,Show)

 
data CPackageBody = 
       CPackageDef [CProp] Position [CLetDef]
     | CPackageInstance CExpr
    deriving (Eq, Ord,Show)
       
data CClause 
        = CClause CExpr
                   --[(Bool,CPat)] CExpr
         deriving (Eq, Ord,Show)

data CPatArg = CPatT Id CExpr
             | CPatId Id
           deriving (Eq, Ord,Show)

getCPatArgPos :: CPatArg -> Position
getCPatArgPos (CPatT x _) = getIdPosition x
getCPatArgPos (CPatId x) = getIdPosition x


data CPat
        = CPCon Id [CPat]  
        | CPVar CPatArg
--        | CPAs Id CPat
--        | CPLit Position Literal
        deriving (Eq, Ord,Show)

getCPatPos :: CPat -> Position
getCPatPos (CPCon c _) = getIdPosition c
getCPatPos (CPVar x) = getCPatArgPos x


data CArg = CArg [(Bool,Id)] CType
        deriving (Eq, Ord,Show)

data CSign
        = CSign [Id] CType
        | CSignDef CDefn
--      | CSignType Id CArgs  ???
        deriving (Eq, Ord,Show)

data CClassArg = CClassArg Id CArgs CExpr CArgs deriving (Eq,Show,Ord)
data CInstanceArg = CInstanceArg CExpr deriving (Eq,Show,Ord)

--data CBind
--      = CBind CArg CExpr
--      | CBind_ CExpr
--      | CBLet [CLetDef]
--         deriving (Eq, Ord)

-- instance PPrint CBind where
--     pPrint d p (CBind a e) = pp d a ~. t" <- " ~. pp d e ~. t";"
--     pPrint d p (CBind_ e) = pp d e ~. t";"
--     pPrint d p (CBLet ds) = t"let " ~. foldr1 (^.) (map (pp d) ds) ~. t";"

{- moved to Id
ppId :: PDetail -> Id -> IText
ppId d i = 
    case getIdString i of
    s@(c:_) | isAlpha c || c == '_' -> t s
    s -> t ("("++s++")")

pprId :: Id -> String
pprId i = pIText (ppId PDReadable i)
-}

{-
ppConId :: PDetail -> Id -> IText
ppConId d i =  
    (case getIdString i of
       s@(c:_) | isAlpha c -> t ('@':s)
       s -> t ('@':("("++s++")")))

-}

{- moved to Id
ppInfix :: PDetail -> Id -> IText
ppInfix d i =
    (case getIdString i of
      s@(c:_) | isAlpha c -> t("`"++s++"`")
      s -> t s)
-}



idCDefn :: CDefn -> Maybe Id
idCDefn (CValueT c _ _ _) = Just c
idCDefn (CValueS c _ _ _) = Just c
--idCDefn (CValueP c _) = Just c
idCDefn (Ctype c _ _) = Just c
idCDefn (Cnewtype c _ _ _) = Just c
idCDefn (Cdata c _ _ _) = Just c
idCDefn (CValue c _) = Just c
idCDefn (CAxiom c _ _) = Just c
idCDefn (CNative c _) = Just c
idCDefn (CPackage c _ _) = Just c
idCDefn (COpen e as) = Nothing
idCDefn (CClass (CClassArg c _ _ _) _ _) = Just c 
idCDefn (CInstance c _ _ _) = Just c
idCDefn (Cnewtype c _ _ _) = Just c
--idCDefn (CNative c _ _) = Just c
--idCDefn (CDSign c _) = Just c               -- Used only while type checking



idCDef :: CDef -> Maybe Id
idCDef (CDefComment _) = Nothing
idCDef (CDef ps d) = idCDefn d



idsCDefn :: CDefn -> [Id]
idsCDefn (CValueT c _ _ _) = [c]
idsCDefn (CValueS c _ _ _) = [c]
--idsCDefn (CValueP c _) = [c]
idsCDefn (Ctype c _ _) = [c]
idsCDefn (Cnewtype c _ _ _) = [c]
idsCDefn (Cdata c _ _ _) = [c]
idsCDefn (CValue c _) = [c]
idsCDefn (CAxiom c _ _) = [c]
idsCDefn (CPackage c _ _) = [c]
idsCDefn (COpen e as) = []
--idsCDefn (CNative c _ _) = [c]
--idsCDefn (CDSign c _) = [c]               -- Used only while type checking


idsCDef :: CDef -> [Id]
idsCDef (CDefComment _) = [] 
idsCDef (CDef _ d) = idsCDefn d

idsCLetDef :: CLetDef -> [Id]
idsCLetDef (CSimple d) = idsCDef d
idsCLetDef (CMutual ds) = concatMap idsCDef ds
idsCLetDef _ = []


addModifiers :: [CProp] -> CDef -> CDef
addModifiers ps (CDef ps' ds)  = CDef (addMod' ps ps') ds
          where  addMod' [] ps = ps
                 addMod' (p:ps) ps' = 
                    let ps2 = addMod' ps ps'
                    in if elem p ps2
                        then ps2
                        else case p of
                                Cabstract | elem Cconcrete ps' -> ps2
 --raise (noPosition,EConflictingModifiers (ppReadable p) "concrete")
                                Cconcrete | elem Cabstract ps' -> ps2
 --raise (noPosition,EConflictingModifiers (ppReadable p) "abstract")
                                Cpublic | elem Cprivate ps' -> ps2
 --raise (noPosition,EConflictingModifiers (ppReadable p) "private")
                                Cprivate |  elem Cpublic ps' -> ps2
--raise (noPosition,EConflictingModifiers (ppReadable p) "public")
                                _ -> (p:ps2)
addModifiers ps d = d

data CConstraint = CEq CExpr CExpr
                 | CJudg (CJudgement CExpr)

data CJudgement a =  CIsType a
                  | HasType a  CExpr
                  --deriving Show

precCExpr :: CExpr -> Int 
precCExpr (CVar _) = 12
precCExpr (CStar _ _ _) = 12
precCExpr (CMeta _ _ _ _) = 12
precCExpr (CSelect _ _) = 12
precCExpr (CConS _ ) = 12
precCExpr (CCon _ _ ) = 12
precCExpr (CBinOp _ op _) = prec $ getFixity op
precCExpr (CClos _ e) = precCExpr e
precCExpr (CArrow _ _ _) = 0
precCExpr _ = 9

type CMetaSubst = (Bool,MetaVar,CExpr)

-- Gives an approx. position
getCExprPos :: CExpr -> Position
getCExprPos e = case e of   
    CVar n -> getIdPosition n
    CStar pos _ _  -> pos
    Clam as e -> getCExprPos e
    CUniv as e -> getCExprPos e
    CArrow _ e1 _ -> getCExprPos e1
    Clet _ e -> getCExprPos e
    CProduct pos _ -> pos
    CRecord _ pos _ -> pos
    Copen m _ _ -> getCExprPos m
    CSelect _ n -> getIdPosition n
    CSum [] -> noPosition
    CSum ((n,_):_) -> getIdPosition n
    CCon n _  -> getIdPosition n 
    CConS n -> getIdPosition n
    Ccase e _ -> getCExprPos e
    CApply e _ -> getCExprPos e
    CBinOp e1 _ _ -> getCExprPos e1
    CMeta pos _ _ _ -> pos 
    CClos _ e -> getCExprPos e
    Ccomment _ _ e -> getCExprPos e
    CPackageType  -> noPosition
    CIndSum _ [] -> noPosition
    CIndSum _ (((n,_),_):_) -> getIdPosition n


boundVars :: CArg -> [Id]
boundVars (CArg hxs _) = map snd hxs
{-- ------------------- -}

