module SwapMetaVars where

import CSyntax
import Monad(ap)
import MetaVars(MetaVar)
import Id(Id)
--import AgdaTrace(trace)
--import PPrint

import SwapMonad -- >>= is never used, so it is something weaker than a monad...

#ifdef __HASKELL98__
#define map fmap
#endif

infixl 1 #,<#

( # )  :: (Functor f) => (a->b) -> f a -> f b
--(#) :: (Eval b) => (a->b) -> SwapMonad m a -> SwapMonad m b
(<#) :: Monad f => f (a->b) -> f a -> f b

( # ) = map
--(#) = smap
( <# ) = ap
-- Eqn: f # x = return f <# x

----
type SwapMetaVars syntax = syntax -> [MetaVar] -> (syntax,[MetaVar])

swapMetaVars :: HasMetaVars s => SwapMetaVars s
swapMetaVars = run . sms

------
type SwapMetaVarsM syntax = syntax -> SwapMonad MetaVar syntax

class HasMetaVars syntax where
  sms ::  SwapMetaVarsM syntax

instance HasMetaVars s => HasMetaVars [s] where
  sms = mapM sms

instance HasMetaVars s => HasMetaVars (Maybe s) where
  sms Nothing  = return Nothing
  sms (Just x) = map Just (sms x)

instance (HasMetaVars s1,HasMetaVars s2) => HasMetaVars (s1,s2) where
  sms (s1,s2) = (,) # sms s1 <# sms s2

instance HasMetaVars CProgram where
  sms (CProgram ds) = CProgram # sms ds

instance HasMetaVars CModule where
  sms (CModule i args pbody) = CModule i # sms args <# sms pbody

instance HasMetaVars CExpr where
  sms e =
    case e of
      Clam   b e        -> Clam     # sms b  <# sms e
      CUniv  b e        -> CUniv    # sms b  <# sms e
      CArrow b e1 e2    -> CArrow b # sms e1 <# sms e2
      Clet ds e         -> Clet     # sms ds <# sms e
      CProduct pos cs   -> CProduct pos # sms cs
      CRecord pos ps ds -> CRecord pos ps # sms ds
      Copen e1 oargs e2 -> Copen    # sms e1 <# sms oargs <# sms e2
      CSelect e i       -> flip CSelect i # sms e
      CSum cs           -> CSum     # sms cs
      CIndSum b cs      -> CIndSum  # sms b  <# sms cs
      CCon i e          -> CCon i   # sms e
      Ccase e arms      -> Ccase    # sms e  <# sms arms
      CApply e es       -> CApply   # sms e  <# sms es
      CBinOp e1 i e2    -> flip CBinOp i # sms e1 <# sms e2
      CMeta pos b vis m -> CMeta pos b vis # swap m
      Ccomment b cmnt e -> Ccomment b cmnt # sms e
      _                 -> return e

instance HasMetaVars Bool where sms = return
instance HasMetaVars Id where sms = return

instance HasMetaVars CDef where
  sms (CDef ps d) = CDef ps # sms d
  sms d           = return d

instance HasMetaVars CLetDef where
  sms (CSimple d)  = CSimple # sms d
  sms (CMutual ds) = CMutual # sms ds
  sms d            = return d

instance HasMetaVars CDefn where
  sms (CValueT i as a e) = CValueT i # sms as <# sms a <# sms e
  sms (Ctype i as a)     = Ctype   i # sms as <# sms a
  sms (Cdata i as mt cs) = Cdata   i # sms as <# sms mt <# sms cs
  sms (CValue i e)       = CValue  i # sms e

  sms (CAxiom i as a)   = CAxiom   i # sms as <# sms a
  sms (CPackage i as e) = CPackage i # sms as <# sms e
  sms (COpen e oargs)   = COpen      # sms e <# sms oargs

instance HasMetaVars CPackageBody where
  sms (CPackageDef ps pos ds) = CPackageDef ps pos # sms ds
  sms (CPackageInstance e)    = CPackageInstance # sms e

instance HasMetaVars CPatArg where
  sms (CPatT i e)   = CPatT i # sms e
  sms p             = return p

instance HasMetaVars CPat where
  sms (CPCon i pas) = CPCon i # sms pas
  sms (CPVar pa)    = CPVar # sms pa

instance HasMetaVars CArg where
  sms (CArg is a)   = CArg is # sms a

instance HasMetaVars CSign where
  sms (CSign i a)   = CSign i # sms a
  sms (CSignDef d)  = CSignDef # sms d

instance HasMetaVars COArg where
  sms (COArgT ps x a)     = COArgT ps x # sms a
  sms (COArgAsT ps n a x) = COArgAsT ps n # sms a <# return x
  sms oa                  = return oa

instance HasMetaVars COpenArgs where
  sms (COpenArgs oas) = COpenArgs # sms oas
  --sms COpenAll = return COpenAll
