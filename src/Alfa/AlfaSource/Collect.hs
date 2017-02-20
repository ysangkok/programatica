module Collect where
import List(groupBy)

collectBySnd x =
    map pick .
    groupBy eqSnd $ x
  where
    pick xys@((_,y):_) = (map fst xys,y)

collectByFst x = map swap . collectBySnd . map swap $ x

onSnd f (_,y1) (_,y2) = f y1 y2
--cmpSnd = onSnd compare
eqSnd x = onSnd (==) x
--onFst f (x1,_) (x2,_) = f x1 x2
--cmpFst = onFst compare
--eqFst = onFst (==)

swap (x,y) = (y,x)

-- eta expansions because of the silly monomorphism restriction
