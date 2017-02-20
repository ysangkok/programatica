module Generators where

import QuickCheck
import ISynType
import Monad

environment :: [Uid] -> Gen Environment
environment vars = zip

-- | Generates a list of 'UId's. No duplicates are returned.

uids :: Gen [UId]
uids = nub $ list uid
  where uid = liftM3 UId undef undef arbitrary

-- | A generator for arbitrary (non-well-typed) expressions containing
-- free variables only in the supplied list.

expr :: [UId] -> Gen Exp
expr vars = sized (exp vars)
  where
  baseCases  = [meta, var, const, sort]
  exp vars 0 = oneof $ map ($ vars) baseCases
  exp vars n = oneof $ map ($ vars) $ baseCases ++ [app n]

  meta _     = liftM5 EMeta undef undef arbitrary undef arbitrary `ap` undef
  var vars   = liftM2 EVar (elements vars) undef
  const vars = liftM2 EConst (elements vars) undef
  sort _     = liftM2 ESort undef undef

  app n vars = do
    let ex = exp (n-1) vars
    e  <- ex
    es <- list $ liftM2 (,) arbitrary ex
    return $ EApp e es

  abstraction n vars = do
    uids' <- uids
    bools <- listOfLen (length uids') arbitrary
    e     <- exp (uids' `union` vars) (n-1)
    return (EAbs (zip bools uids', undefined) e

-- Various helper functions.

listOfLen n gen = sequence $ replicate n gen
list gen = do
  n <- arbitrary
  listOfLen (abs n) gen

undef = return undefined

{-
data Exp = 
   EMeta    MetaVar Position Bool TransClass Int Visibility
 | EMetaV    MetaVar Bool FCVars
 | EVar      UId (Maybe TransClass)
 | EConst    UId (Maybe TransClass)
 | EConstV   UId FCVars
 | ESort     Position Sort
 | EProd     Bind  Exp
 | EArrow    Bool Exp Exp
 | EAbs      Bind  Exp 
 | EApp      Exp  [(Bool,Exp)]
 | EBinOp    Exp   Exp Exp                                -- EBinOp e1 op e2
 | EIf       Exp Exp Exp
 | EDef      [LetDef] Exp
 | EOpen     Exp OpenArgs Exp
 | ESig      Position [ESigDef]
 | EStruct   Position [LetDef] FCVars [UId] [UId]       -- pub and abs const
 | EPackageType
 | Epackage  Position [LetDef] FCVars [UId] [UId]      -- pub and abs const
 | EProj     Exp Id
 | EData     [ConBind]
 | EIndData  Tel [IndConBind]          -- Tel is [] when used for 1.
 | ECon      Id     [(Bool,Exp)] 
 | EConF     Id Exp [(Bool,Exp)] 
 | ECase     Exp [(CaseBranch,Exp)]
 | PreMeta 
 | EStop MetaVar Exp
 | EClos Environment Exp  
 | ELiteral Position Literal 
-}
