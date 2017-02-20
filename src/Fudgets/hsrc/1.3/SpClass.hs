module SpClass where
import EitherUtils(stripEither)

-- Preliminary class structure for stream processors

-- operator names?

class AtomicStreamProc sp where -- SPIO SP_IO SpIO SpIo ?
  put :: o -> sp i o -> sp i o
  get :: (i -> sp i o) -> sp i o
  end :: sp i o

puts = foldr ((.).put) id

-- It would be nice to be able to reuse the name map here

--map' :: (AtomicStreamProc sp) => (a->b) -> sp a b

map' f = mapf where mapf = get $ \ x -> put (f x) $ mapf

--toBoth :: (AtomicStreamProc sp) => sp a (Either a a)
toBoth = get $ \ x -> puts [Left x,Right x] $ toBoth

class AtomicStreamProc sp => StreamProc sp where
  -- really inherit AtomicStreamProc ?
  --- methods
  ser :: sp m o -> sp i m -> sp i o
  preMap :: sp m o -> (i->m) -> sp i o
  postMap :: (m->o) -> sp i m -> sp i o
 
  comp :: sp i1 o2 -> sp i2 o2 -> sp (Either i1 i2) (Either o1 o2)
  par :: sp i o -> sp i o -> sp i o

  loopComp :: sp (Either (Either a b) (Either c d))
                 (Either (Either c e) (Either a f)) ->
              sp (Either b d) (Either e f)

  loop :: sp a a -> sp a a
  loopLeft :: sp (Either l i) (Either l o) -> sp i o

  --- defaults
  sp `preMap` f = sp `ser` map' f
  f `postMap` sp = map' f `ser` sp

  sp1 `par` sp2 = stripEither `postMap` comp sp1 sp2 `ser` toBoth

  loop sp = loopLeft (toBoth `ser` sp `preMap` stripEither)
