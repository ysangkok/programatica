{-+
Type environments.

For efficiency, a type environment is represented as a pair of an environment
and the set of (nongeneric) type variables that occur free in the environment.
The type checker needs this information when it generalizes a type,
and traversing the whole environment every time is too costly.
-}
module TiTEnv(TEnv,extenv1,extenv,empty,TiTEnv.lookup,domain,range) where
import Sets

import HsIdent(HsIdentI)
import TiTypes(Scheme,Types(..))
import TiNames(TypeVar)
import qualified TiEnvFM as E


type TEnv' i = E.Env (HsIdentI i) (Scheme i) -- types of value identifiers
data TEnv i = TEnv (TEnv' i) (Set i)

extenv1 x t (TEnv env vs) = TEnv (E.extenv1 x t env) (vs `union` mkSet (tv t))
extenv bs1 (TEnv bs2 vs) = TEnv (E.extenv bs1 bs2) (vs `union` mkSet (tv (map snd bs1)))
empty = TEnv E.empty emptySet

lookup (TEnv env _) x = E.lookup env x
domain (TEnv env _) = E.domain env
range (TEnv env _) = E.range env

--instance Functor TEnv where ...

instance TypeVar i => Types i (TEnv i) where
  tv (TEnv env vs) = setToList vs
  -- tmap -- of questionable use...
