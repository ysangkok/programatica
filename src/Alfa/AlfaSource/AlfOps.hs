module AlfOps where

--- Proof engine stuff

--import ProofmonadOps
import qualified ProofEngine as PE
import UAbstract
import FileConv(parse,parse')
--import AbstractOps
import AlfModules -- (Module(..),Import(..),cdecl',CDecl(..),mapCDecl,mapCDecls,stripCDecls)
--import qualified ForgetMetas(module')

--- Alfa stuff
import AlfSyntax
import SubstMeta
import EditFunc(EditFunc,editPlaceHolders,editExternal,editGlobal)
import EditMonad
import AlfEditMonad
import EditAnnot
import Gedit(build)
import DrawAlf
import DrawOptions
import AlfState
import AlfaText() -- type synonyms
import AlfaPlugin(CompleteFunc) -- + type synonyms
--import Load
--import AlfPrint(prSyntax1line,prExp1line)

--- Util stuff
--import Exception
import ListUtil(assoc)
import HO(apFst,apSnd)
import Utils2(apBoth)
import Monad(ap)

--- Fudgets stuff
import Fud(Gfx,replacePart,deletePart)

--- debugging:
--import Debug2(trace)

#ifdef __HASKELL98__
#define map fmap
#endif

drawAlfTopSyntax state = drawTopSyntax (altdispfs state) (drawOptions state)
drawAlfSyntax    state = drawSyntax    (altdispfs state) (drawOptions state)

type AlfEditFunc = EditFunc AlfAnnot AlfState Syntax AlfEditMsg

data AlfEditMsg
  = RefineWithText
      Bool -- multiline
      String -- prompt
      (Maybe String,String,TextInputParser,Bool) -- (default,example,immediate)
      CompleteFunc -- completions
  | Copy Syntax
  | Paste (Either String Syntax->AlfEditFunc)
  | EditLayout (Name,ArgOptions)
  | BrowseFile FilePath
  | SaveFileAs (Maybe FilePath) String
  | DisplayGfx Gfx

editByPasting = editExternal . Paste

type TextInputParser = String -> Either ((Int,Int),String) AlfEditFunc
noCompletions = const []

editWithParsedText s = editWithParsedTextEx s ""
editWithSlowParsedText s = editWithParsedTextEx'' False True s ""

editWithParsedString s = editWithParsedStringEx s ""
editWithParsedStringEx = editWithParsedTextEx' False
editWithParsedTextEx = editWithParsedTextEx' True
editWithParsedTextEx' = editWithParsedTextEx'' True

editWithParsedTextEx'' imm m s ex efunc =
   editExternal (refineWithText' m s (Nothing,ex,efunc,imm)
		                 noCompletions) :: AlfEditFunc


editWithParsedStringDef = editWithParsedTextDef' False
editWithParsedTextDef = editWithParsedTextDef' True
editWithParsedTextDef' m s def efunc =
   editExternal (refineWithText' m s (Just def,"",efunc,True)
		                 noCompletions) :: AlfEditFunc


--refineWithString = refineWithText' False -- single line input
--refineWithText = refineWithText' True -- multiple line input

editWithText ml prompt parse compl =
  editExternal (refineWithText' ml prompt parse compl)

refineWithText' multiline s (def,ex,efunc,imm) compl =
    RefineWithText multiline s (def,ex,dropfilename . efunc,imm) compl
  where
    dropfilename = either (Left .  apFst (apBoth (1+) .snd)) Right

browse = editExternal . BrowseFile
editLayout = editExternal . EditLayout
saveFileAs optpath = editExternal . SaveFileAs optpath
displayGfx = editExternal . DisplayGfx

--parsedInput :: Parse a -> (a->AlfEditFunc) -> TextInputParser
--checkedInput :: Parse a -> (String->AlfEditFunc) -> TextInputParser

--readInput :: Read a => (a->AlfEditFunc) -> TextInputParser
readInput cont str =
  case reads str of
    [(x,[])] -> Right (cont x)
    t -> Left (("",(0,0)),"??"{-"An "++showType (fst $ head $ t)++" is required"-})

parsedInput p cont = parsedInput' p (const cont)
checkedInput p cont = parsedInput' p (\s _ -> cont s)

parseStrEd p = liftEitherEd . parse p

parsedInput' p cont str =
  case parse' p str of
    Right x -> Right (cont str x)
    Left (pos,msg) -> Left (pos,msg)

editReplace x = editGlobal . replaceSyntax $ x

replaceSyntax syntax path drawing _ =
  do state <- loadEd
     let drawing' = replacePart drawing path (drawAlfTopSyntax state syntax)
     reload drawing'

delElem path olddrawing _ = reload (deletePart olddrawing path)

reload drawing = reloadModuleDecls ({-ForgetMetas.module'-} mod)
  where
    ModuleS mod = build drawing

    -- reload declarations, preserve imports:
    reloadModuleDecls (Module cdecls) =
	do --pres <- map (preserve.proofState) loadEd
	   savedPlugs <- savePluginsEd
	   dropThisModuleEd
	   --updateEd (reinstall pres)
	   --cdecls' <- cstepsEd cdecls
	   --cdecls'' <- edPE (adjustCDeclsG cdecls')
	   cdecls'' <- appendCDecls cdecls
	   let newmodule = Module cdecls''
	   reloadPluginsEd newmodule savedPlugs
	   return [([],ModuleS newmodule)]
    {-
      where
	preserve ps =
	    (getProofGoalCounter ps,theories)
	  where theories = map (apSnd ms) (getTheories ps)
		ms = comSubstMeta (getProofMetaSubst ps)
	reinstall (gcnt,theories) =
	    changeProofState (putTheories theories. putProofGoalCounter gcnt)
			      -- discard deleted theories !!
    -}

newtype Refinement a = R a

buildRefinementG (R e) = return e

editRefine (R e) = editMetaGive e
editMetaGiveG eG = editMeta ((eG>>=) . flip refineGiveG)
editMetaGive = editMeta . refineGiveG
refineGiveG = refineGoalG . flip PE.give

editMeta rG = editPlaceHoldersG (rG.apFst expSmeta)
  where expSmeta (ExpS (EMeta g)) = g

--editGoalG :: (Int->G ())->AlfEditFunc
editGoalG = editMeta . refineGoalG

editPlaceHoldersG g = editPlaceHoldersG' g'
  where g' ((path,syntax):phs) =
	   do (syntax',phs') <- g (syntax,phs)
	      return ((path,syntax'):phs')

editPlaceHoldersG' g = editPlaceHolders (edPE.g)

refineGoalG rG (g,phs) = rG g >> adjustMetaG g phs
  where
    adjustMetaG g phs =
      do ms <- PE.metaSubst
	 --e <- lookupG ms g
	 return (ExpS (recordSubstMeta ms (EMeta g)),substPhs ms phs)

editMetas = editPlaceHoldersG' . instantiatePlaceHoldersG
  where
    instantiatePlaceHoldersG inst phs = inst >> adjustMetaG phs

    adjustMetaG phs =
      do ms <- PE.metaSubst
	 return (substPhs ms phs)

substPhs ms [] = []
substPhs ms ((path,(ExpS (EMeta g))):phs) =
  assoc (\e->((path,ExpS (recordSubstMeta ms e)):)) id ms g (substPhs ms phs)
substPhs ms (someOtherKindOfPlaceHolder:phs) = substPhs ms phs

loadParsedModule imported (Module cdecls) =
    do --cdecls' <- cstepsEd cdecls
       --cdecls'' <- edPE (adjustCDeclsG cdecls')
       cdecls'' <- appendCDecls' imported cdecls
       return (ModuleS (Module cdecls''))

appendCDecls = appendCDecls' False

appendCDecls' imported cdecls =
    do cdecls' <- cstepsEd cdecls
       edPE (adjustCDeclsG cdecls')
  where
    cstepsEd [] = return []
    cstepsEd (cd:cdecls) =
      case cd of
	Comment c | take 10 c==errprefix -> cstepsEd cdecls
		   | otherwise -> map (cd:) (cstepsEd cdecls)
	ImportDecl _ -> map (cd:) (cstepsEd cdecls)
	Decl _ d ->
	   optHandleErr d $
	     do --(ds',_) <- stepsEd [cd]
	        (ds',_) <- edPE (PE.appendDecls [cd])
	        map (ds'++) (cstepsEd cdecls)
      where
        optHandleErr d cmds =
	  if imported
	  then cmds
	  else
	    cmds
	   `handleEd` \ msg ->
	    return (Comment (errprefix++msg++"-}"):Decl False d:skipRest cdecls)
	skipRest = map makebad
	makebad (Decl _ d) = Decl False d
	makebad cd = cd
	errprefix = "{- Error: "

    adjustCDeclsG cdecls =
	recordSubstMeta `map` PE.metaSubst `ap` return cdecls

elimEd state ed ok err = either err ok (runEd ed state)

--tr x = trace (show x) x

lookupG xs x = liftMaybeEd "lookupG" (lookup x xs)
