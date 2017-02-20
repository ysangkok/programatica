module AlfParse(  
  --parse,parse',lexanal,
  --PosTOKEN(..),TOKEN(..),Pos(..),
  --apP,unitP,manyP,chkP,theRestP,mapP,star,optionalP,someP,
  --symbolP',idenP',parse_decl,parse_exp,parse_ctx,parse_Name,commentsP,
  --ismyAlphanum,
  --parseModule,--declsP,parseDecls,
  parseImport
  ) where
import AlfParserMonad
--import UParse
import AlfModules
--import UAbstract
import AlfLex

--importsP = manyP importP
importP = unitP Import `chkP` includeP `apP` idenP `chkP` manyP commentP
  where
    includeP = maybeP (checkP (Iden "--")) `chkP`  symbolP "#include"

--parseImports = parse (unitP (,) `apP` importsP `apP` theRestP) . lexanal

parseImport = parse importP . lexanal

{-
parseModule = parse moduleP . lexanal

moduleP = unitP Module `apP` importsP `apP` declsP

parseDecls = parse declsP

declsP = concat `mapP` star declP

--declP :: Parse ExpAnnot [CDecl]
declP = unitP join `apP` commentsP `apP` optDeclP `apP` commentsP
  where
    optDeclP = optionalP [] (mapP (:[]) parse_decl)
    join cs1 ds cs2 = map Comment' cs1++map cdecl' ds++map Comment' cs2
-}
