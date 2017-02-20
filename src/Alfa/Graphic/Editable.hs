module Editable where
import Drawing

-- An attempt to define a class of editable data structures.
-- This works only if all parts in the data structure have the same type,
-- which is typically NOT the case for abstract syntax, unfortunately.

class Editable a where
  draw :: a -> ([a], [Drawing] -> Drawing)
  operations :: a -> [(a->a,String)]
  part :: a -> [Int] -> a
  replacePart :: a -> [Int] -> (a->a) -> a

--data E = Editable b => E b
