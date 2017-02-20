module AgdaParse where
import qualified Agda
import qualified ConvFromAgda as U
import AlfParse(parseImport)
import AlfModules

parseModule filename contents =
    case parseDecls contents of
      Right decls0 -> Right (Module decls)
       where decls = map parseImport' decls0
      Left err -> Left err
  where
    parseImport' cd@(Comment s) =
      case parseImport s of
        Right i -> ImportDecl i
	Left _ -> cd
    parseImport' cd = cd

    parseDecls = Agda.run . Agda.sParseFile filename declsP

---

type Parser a = Agda.CParser a

declsP = U.decls `mapP` Agda.pLetDefs
expP = U.exp `mapP` Agda.pExpr
ctxP = U.context' `mapP` Agda.pPArgs
varP = U.var `mapP` Agda.pId
conP = U.con `mapP` Agda.pId
labelP = U.label `mapP` Agda.pId

mapP :: (a->b) -> Parser a -> Parser b
someP,manyP,commaSeqP :: Parser a -> Parser [a]

mapP = flip (Agda.>>-)
manyP = Agda.many
someP = Agda.many1
commaSeqP p = p `Agda.sepBy` Agda.cm

parse p = Agda.run . Agda.sParse p
parse' p = Agda.run' . Agda.sParse p
