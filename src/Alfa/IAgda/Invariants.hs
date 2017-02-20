module Invariants where

import ISynType
import Eval
import Monads
import ProofState
import QuickCheck
import Generators

-- | @'isValue' e@ is 'True' if @e@ is a value.
--
-- Note that we are unsure if this definition is correct.

-- Check comment in ISynType, close to the definition of Value.

isValue :: Exp -> Bool
isValue e = case e of
 EMeta {}         -> False
 EMetaV {}        -> False
 EVar _ Nothing   -> True
 EVar _ _         -> error "isValue: EVar _ (Just _): Don't know."
 EConst {}        -> True -- TODO: Fix.
 EConstV {}       -> True -- TODO: Fix.
 ESort {}         -> True
 EProd {}         -> False
 EArrow _ e1 e2   -> isValue e1 && isValue e2
 EAbs {}          -> False
 EApp e bes       -> isNeutralValue e && all isValue (map snd bes)
 EBinOp e1 op e2  -> isNeutralValue op && all isValue [e1, e2]
 EIf test the els -> isNeutralValue test && all isValue [the, els]
 EDef {}          -> False
 EOpen {}         -> False
 ESig {}          -> False
 EStruct {}       -> False
 EPackageType     -> True
 Epackage {}      -> False
 EProj e _        -> isValue e
 EData {}         -> False
 EIndData {}      -> False
 ECon _ bes       -> all isValue $ map snd bes
 EConF _ e bes    -> isValue e && all isValue (map snd bes)
 ECase {}         -> False
 PreMeta          -> error "isValue: PreMeta: Don't know."
 EStop {}         -> True
 EClos _ exp      -> isIntro exp
 ELiteral {}      -> error "isValue: ELiteral: Don't know."

-- | @'isValue' e@ is 'True' if @e@ is in introduction form.

isIntro :: Exp -> Bool
isIntro e = case e of
  EData {}    -> True
  EIndData {} -> True
  EStruct {}  -> True
  Epackage {} -> True
  EAbs {}     -> True
  EProd {}    -> True
  _           -> False

-- | @'isNeutralValue' e@ is 'True' if @e@ is a neutral value. Note
-- that meta-variables are treated as neutral values.

isNeutralValue :: Exp -> Bool
isNeutralValue e = case e of
 EMeta {}         -> True
 EMetaV {}        -> True
 EVar _ Nothing   -> True
 EVar _ _         -> error "isNeutralValue: EVar _ (Just _): Don't know."
 EConst {}        -> True -- TODO: Fix.
 EConstV {}       -> True -- TODO: Fix.
 EApp e bes       -> isNeutralValue e && all isValue (map snd bes)
 EBinOp e1 op e2  -> isNeutralValue op && all isValue [e1, e2]
 EIf test the els -> isNeutralValue test && all isValue [the, els]
 EProj e _        -> isNeutralValue e
 ECase e ces      -> True -- TODO: Fix me.
 PreMeta          -> error "isValue: PreMeta: Don't know."
 EStop {}         -> True
 ELiteral {}      -> error "isValue: ELiteral: Don't know."
 _                -> False

-- | Test that the output of 'eval' is a value.

prop_eval_value :: Property
prop_eval_value =
  forAll expr $ \exp ->
    forAll (environment exp) $ \env ->
      case run (eval env exp) of
      Nothing -> False
      Just (v, _s) -> isValue v

-- | Run a computation in the state monad using 'ProofState.initState'
-- as the initial state.

run :: StateM State a -> Maybe (a, State)
run m = case funSTM m initState of
  Err _  -> Nothing
  Done r -> Just r

-- | All the tests in this module.

tests :: IO ()
tests = do
  test prop_eval_value
