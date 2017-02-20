
-------- Declarations ------------------------------------------------------

module HsDeclStruct where

import SrcLoc
import HsIdent
import HsGuardsStruct
import HsAssocStruct

-- DI
--   i      identifiers
--   e      expression recursion type
--   p      pattern recursion type
--   ds     declaration recursion type
--   t      type recursion type
--   c      context recursion type
--   tp     type pattern recursion type
-- This type seems to be full of awkward inconsistencies... /TH
data DI i e p ds t c tp
    = HsTypeDecl    SrcLoc   tp t
    | HsNewTypeDecl SrcLoc c tp (HsConDeclI i t c) {-deriving-} [i]
    | HsDataDecl    SrcLoc c tp [HsConDeclI i t c] {-deriving-} [i]
    | HsClassDecl   SrcLoc c tp (HsFunDeps i) {-where-} ds
    | HsInstDecl    SrcLoc (Maybe i) c t {-where-} ds -- optionally named
    | HsDefaultDecl SrcLoc [t]
    | HsTypeSig     SrcLoc [i] c t
    | HsFunBind     SrcLoc [HsMatchI i e p ds]
    | HsPatBind     SrcLoc p (HsRhs e) {-where-} ds 
    | HsInfixDecl   SrcLoc HsFixity [HsIdentI i] -- Haskell 98

    -- Hugs compatibility:
    | HsPrimitiveTypeDecl SrcLoc c tp -- data T a1 ... an;
    | HsPrimitiveBind     SrcLoc i t -- primitive f :: t
      deriving (Eq, Show)

data HsMatchI i e p ds
    = HsMatch SrcLoc i [p] (HsRhs e) {-where-} ds   
      deriving (Eq, Show)
  
data HsConDeclI i t c 
    = HsConDecl SrcLoc [i] c i [HsBangType t]
    | HsRecDecl SrcLoc [i] c i [([i], HsBangType t)]
      deriving (Eq, Show)   
   
data HsBangType t 
    = HsBangedType t
    | HsUnBangedType t
      deriving (Eq, Show)   

type HsFunDeps v = [HsFunDep v] -- ..., a b-> c d,f->g h, ->i, ...
type HsFunDep v = ([v],[v]) -- a b-> c d


