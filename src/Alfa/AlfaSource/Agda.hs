module Agda(
  module BasicOps,
  module CSyntax,
  module ISyntax,
  module CParser,
  module CPrinter,
  module Parse,
  module Position,
  module Id,
  module ProofMonad,
  module ProofState,
  module Monads,
  --module Error,
  module RemMetaVarsThomas,
  module PPrint,
  --module CITrans,
  forgetMetaVars,
  addDefs',
  extractMetaSubst,
  extractMetaVars,
  metaContext,
  metaType,
  typeOf,
  convError,
  convPosError,
  nontermError,
  run,
  run',
  sParseFile,
  sParse',
  sParse)
where
import BasicOps
import CSyntax
import CPrinter
import PPrint(ppReadable,ppAll,ppDebug,PPrint(..),PDetail(..),pIText)
import ISyntax(Value(..),Exp,Judgement(..),getUIdPosition)
import Id(Id,getIdString,getIdPosition,getUIdString)
import CParser(CParser,pId,pLetDefs,pExpr,pPArgs,cm)
--import Commands(pParse)
import Parse((>>-),many,many1,sepBy) -- more...
import Position(Position(..),noPosition)
import ProofMonad(PCM(..),getMetaSubst,getMetaInfo)
import ProofState(State,initState,readMetaVarState,writeTermCounter,defaultCounter)
import Monads(StateM(..),Error(..),runSTM)
import Error(EMsg,ErrMsg(..),prEMsg,eMsg)
import MetaVarState(readUnboundMetaVars,readMetaSubst,Constraint(..), MetaSubsts)
import RemMetaVarsThomas
--import CITrans(varScope)
import SimpleICTranslate(translate)
--import ExpandExp(expandJudgement)

-- For pParse:
import ProofMonad(getStrTable,putStrTable)
import Monads(liftESTM)
import CParser(finalP)
import Lex(lx)

import ListUtil(mapSnd)

import qualified AltIntMap(IntMap, toList)
import Util(snd3)

#ifdef __HASKELL98__
#define map fmap
#endif

forgetMetaVars x  = fst . remMetaVars $ x

sParseFile filename = sParse' (Position filename 1 0)
sParse = sParse' (Position "" 1 0) -- position missing from module Position

sParse' startPosition p = pParse p startPosition
  where
    --Copy of pParse from Agda/Commands.hs
    pParse p (Position s l c) inp= 
		  do tab <- getStrTable 
		     let lxres = lx True s l c tab inp
		     (a,tab') <- liftESTM (finalP p lxres)
		     putStrTable tab'
		     return a

addDefs' :: [CLetDef] -> PCM [CLetDef]
addDefs'
  = map project_CSyntax_LetDefs . addDefs
  where
    -- PJ: For Agda before summer 2004, use fst instead
    --project_CSyntax_LetDefs :: (a,[CLetDef],c) -> [CLetDef]
    --project_CSyntax_LetDefs = snd3
    -- Apparently, this has changed back... /TH 2004-12-14
    project_CSyntax_LetDefs = fst

-- PJ: translate, but keep hidden arguments
translate' :: Exp -> CExpr
translate' = translate remove_Agdahidden_arguments
  where remove_Agdahidden_arguments = False

type MetaSubsts' = [(Int,CExpr)]
extractMetaSubst :: Monad m => State -> m MetaSubsts'
extractMetaSubst s =
    return (mapSnd translate' (AltIntMap.toList ms))
  where ms :: AltIntMap.IntMap Exp
        ms = readMetaSubst (readMetaVarState s)

extractMetaVars = readUnboundMetaVars . readMetaVarState

metaContext m = map conv `map` getContext m 
  where
    conv (AnArg x t) = conv' x (Just t)
    conv (ABind x t e) = conv' x (Just t)
    conv (APackage x) = conv' x Nothing
    conv' x t = (getUIdString x,t)

metaType i m = conv `map` getTypeOfMeta i m
{-    do (_,cit,_,_,_,j) <- getMetaInfo m
       j' <- {-if i == 0 then-} return j {-else unfoldJudg j-}
       --j2 <- expandJudgement [] j'
       return (conv j')-}
  where conv (CIsType g) = Nothing
	conv (HasType g e) = Just e

typeOf optm e = getTypeExp 0 optm e


convError e =
  case e of
    Done x -> Right x
    Err eMsg -> Left (prEMsg eMsg)

convPosError e =
  case e of
    Done x -> Right x
    Err (Position f l c,errMsg) -> Left ((f,(l,c)),show errMsg)

nontermError (nontermids,nonposids) =
    case es2 of
      []  -> Nothing
      [e] -> Just (prEMsg e)
      _   -> Just (prEMsg (eMsg  noPosition (EMany es2)))
  where
    es  = map mkTermError nontermids
    es' = map mkPosError nonposids
    es2 =  es ++ es'

    --mkTermError :: UId -> EMsg
    mkTermError x = eMsg (getUIdPosition x) (WNotTerminate (ppReadable x))
    --mkPosError :: ([UId],UId) -> EMsg
    mkPosError (fs,f) = eMsg (getUIdPosition f) (WNotPositive (map ppReadable fs) (ppReadable f)) 

--
run' = convPosError . runSTM'
run = convError . runSTM'

runSTM' m = runSTM m initState
