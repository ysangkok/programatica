{-# COMPILERFLAGS -fno-overload-restr #-}
module Monads(ReactionM,react,put,set,get,update,nop) where

#ifdef __HASKELL98__
#define map fmap
#endif

-- Writer & State monad
newtype ReactionM s o a = M (s -> [o] -> (s,[o],a))

instance Functor (ReactionM s o) where
  map f m = do x <- m; return (f x)

instance Monad (ReactionM s o) where
  return x = M (\s o->(s,o,x))
  (M f1) >>= xm2 =
    M $ \ s0 o0 ->
      let (s1,o1,x1) = f1 s0 o2
          M f2 = xm2 x1
          (s2,o2,x2) = f2 s1 o0
      in (s2,o1,x2)

react (M f) s0 = case f s0 [] of (s,o,_) -> (s,o)
put o = M $ \ s os -> (s,o:os,())
set s = M $ \ _ os -> (s,os,())
get = M $ \ s os -> (s,os,s)
update f = M $ \ s os -> (f s,os,())
nop = return ()
nop :: Monad m => m ()
