{-# LANGUAGE NoImplicitPrelude #-}
{-+
This module implements environments (symbol tables) as finite maps.
Finite maps are not necessarily faster than simple association lists, since
although lookups change from O(n) to O(log n), extension changes from
O(1) to O(log n), and the latter cost can be the dominating cost...
-}
module TiEnvFM(Env,extenv1,extenv,empty,lookup,domain,range) where
import Prelude hiding (lookup) -- for Hugs
import qualified Prelude -- Haskell report change workaround
import FiniteMap
--import PrettyPrint(Printable(..),fsep) -- for debugging

data (Ord key) => Env key info = Env (FiniteMap key info)

extenv1 x t (Env bs) = Env (addToFM bs x t)
extenv bs1 (Env bs2) = Env (addListToFM bs2 bs1)
empty = Env emptyFM

lookup (Env env) x = lookupFM env x
domain (Env env) = keysFM env
range (Env env) = eltsFM env


---- Why isn't there a Show instance for FiniteMap?!
--instance (Show key,Show info) => Show (Env key info) where
--  showsPrec n (Env env) = showsPrec n (fmToList env)
--
---- Why isn't there a Functor instance for FiniteMap?!
--instance Functor (Env key) where
--  fmap f (Env bs) = Env (mapFM (const f) bs)

{-
-- For debugging:
instance (Printable key,Printable info) => Printable (Env key info) where
  ppi (Env env) = fsep (keysFM env)
-}
