module Variables where

data Variable state value
  = Vari { set :: value -> state -> state,
           get :: state -> value }

liftState proj inj Vari { set=set0,get=get0 } =
  Vari { get = get0 . proj,
         set = \ v s -> inj (set0 v (proj s)) s }

compVar v1 = liftState (get v1) (set v1)
{-
compVar Vari { set=set1, get=get1 } Vari { set=set2, get=get2 } =
  Vari { get = get2 . get1,
	 set = \ v s -> set1 (set2 v (get1 s)) s }
-}

update v f s = set v new s
  where new = f (get v s)

updateM v f s =
  do new <- f (get v s)
     return (set v new s)
{-
  case f (get v s) of
    Nothing -> Nothing
    Just new -> Just (set v new s)
-}
