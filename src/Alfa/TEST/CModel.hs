module CModel (cm) where
import BddTable 
import Bdt (Bdd(..), bdt)

type Var = Int
type Model = [Signed Var]

data Signed a =
   Neg a
 | Pos a
 deriving (Eq, Ord)

instance Show a => Show (Signed a) where
  showsPrec _ svar =
      case svar of
         Neg a -> shows a
                . showChar ':'
                . showChar '0'
         Pos a -> shows a
                . showChar ':'
                . showChar '1'   


cmodels :: Bdd -> [Model]
cmodels bdd = find 0 bdd [] []
  where
    find v bdd init =
       case bdd of
          O -> (init :)
          I -> id
          Pair l h -> find (v+1) l (Pos v : init) . find (v+1) h (Neg v : init)

cm t = cmodels (bdt t)

