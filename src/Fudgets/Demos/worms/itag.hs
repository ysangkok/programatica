module itag(tag_input) where
import Keys
import ListMap(lookupWithDefault)

--export tag_input,

--local

  -- This is an association list: (char,(userno,stdkey))
keyassignments = concatMap (\(n,s)->zip s (zip (replicate 6 n) stdkeys))
  	           [(1,stdkeys),(2,"wadxzc"),(3,"tfhv56"),(4,"846213")]
--in

tag_input c =
  if c==' ' then Tick else	-- testing
  let (n,k)=lookupWithDefault keyassignments (1,c) c
  in Key n k
