module StateMonads where
import Fudget(K,KEvent)
import FudgetIO
import StreamProcIO
import EitherUtils(Cont(..))
import NullF(getK)

--------------------------------------------------------------------------------
-- The continuation monad:
#ifdef __HBC__
newtype Mk a b = Mk (Cont a b)
unMk (Mk mk) = mk
#else
newtype Mk a b = Mk {unMk::Cont a b}
#endif
type Mkc a = Mk a () -- continuation monad with unit result

instance Functor (Mk a) where
  fmap f (Mk m) = Mk (\k -> m (k.f))

instance Monad (Mk a) where
  return r =  Mk ($ r)
  Mk m1 >>= xm2 = Mk (m1 . flip (unMk . xm2))

--------------------------------------------------------------------------------

-- Continuation monad with state (just an instance of the continuation monad):
type Ms a b c = Mk (b -> a) c
type Msc a b = Ms a b ()

loadMs  :: Ms a b b
storeMs :: a -> Msc b a
modMs   :: (a -> a) -> Msc b a
fieldMs :: (a -> b) -> Ms c a b

loadMs    = Mk (\ k s -> k s s)
storeMs s = Mk (\ k _ -> k () s)
modMs   f = Mk (\ k s -> k () (f s))
fieldMs r = Mk (\ k s -> k (r s) s)

nopMs :: Msc a b
nopMs = return ()

--------------------------------------------------------------------------------

toMkc :: (a -> a) -> Mkc a
toMkc k = Mk (\f -> k (f ()))

toMs :: Cont a b -> Ms a c b
toMs f = Mk (bmk f)
bmk f = (f .) . flip

toMsc :: (a -> a) -> Msc a b
toMsc k = Mk (\f -> k . f ())

--------------------------------------------------------------------------------
--- Ks : Fudget Kernel Monad with State (just an instance...)

type Ks i o s ans = Ms (K i o) s ans
--type Ksc i o s = Ks i o s ()

{-
putsKs :: [KCommand a] -> Ksc b a c
putKs  :: KCommand a -> Ksc b a c
getKs  :: Ks a b c (KEvent a)
nullKs :: Ks i o s ()
loadKs :: Ks i o s s
storeKs :: s -> Ks i o s ()
-}
putHighsMs c = toMsc (puts c)
putHighMs  c = toMsc (put c)
putLowsMs  c = toMsc (putLows c)
putLowMs   c = toMsc (putLow c)
getKs        = toMs getK


-- Some synonyms, kept mostly for backwards compatibility
nullKs   =  nopMs
storeKs  = storeMs
loadKs   = loadMs
unitKs x = return x
bindKs m1 xm2 = m1>>=xm2
thenKs m1 m2 = m1>>m2
mapKs f = fmap f

--stateMonadK :: s -> Ks i o s ans -> (ans -> K i o) -> K i o
stateMonadK s0 (Mk ks) k = ks (\ans state->k ans) s0

--stateK :: a -> (Ksc b c a) -> (K b c) -> K b c
stateK s (Mk ksc) k = ksc (const (const k)) s
