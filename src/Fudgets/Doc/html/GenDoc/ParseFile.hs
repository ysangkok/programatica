module ParseFile(parseFile,Error(..)) where
import Syntax
import Parser
--import Either
--import Maybe
import ParsOps

parseFile = interfacesParser

{- old:
parseFile s =
  case interfacesParser s of
    Right is -> Just is
    _ -> Nothing
-}
