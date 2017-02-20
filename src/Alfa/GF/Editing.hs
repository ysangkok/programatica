module Editing where

import Operations
import Grammar
import SymbolTable
import Macros
import ComputeTerm (compute)
import TypeCheck
import TC (Constraints)
import Refine
import LocalUndo (removeSubTerm)
import BottomUp (wrapTermInfoWithFun, functionsOnType)
import CustomCommands (CommandId, termCommand, allTermCommands)
import Option (Option())

-- AR 5/2/2001

newtype EState = EState [ECurrent]

type ECurrent    = ([TermInfo], (EStateMsg,EOptions))
type ActMeta     = (MetaSymb, Refinements)
type WrapInfo    = [(Int,((Fun,Type),[Int]))]
type Refinements = [(Int,(Trm,Type))]
type EStateMsg   = [String]
type EOptions    = [Option]
type Action      = EState -> EState

emptyEState = EState []

-- basic actions on state

eRefine      :: AbstractST -> (MetaSymb,Trm) -> Action
eChoose      :: Int -> Action
eUndo        :: Action
eLocalUndo   :: AbstractST -> Int -> Action
eLocalWrap   :: AbstractST -> Int -> (Fun,[Int]) -> Action
eWrap        :: AbstractST -> ((Fun,Type),[Int]) -> Action
eTransform   :: AbstractST -> CommandId -> Action
eReset       :: Action
eActivate    :: MetaSymb -> Action
eAddConstrs  :: AbstractST -> Constraints -> Action

-- the following could be derived but are handy to have available
eNew         :: AbstractST -> Cat -> Action
eRefines     :: AbstractST -> [(MetaSymb,Trm)] -> Action
eRefineRef   :: AbstractST -> (MetaSymb,(Trm,Type)) -> Action
eRefineAct   :: AbstractST -> Int -> Action
eWrapAct     :: AbstractST -> Int -> Action
eDummy       :: Action
eMessage     :: String -> Action
eAmbigRefine :: AbstractST -> [(MetaSymb,Trm)] -> Action

eRefine abs (m,t) = 
  actionIf 
      "no refinement possible" 
      [EWork] 
      (changeTermInfo (\ti -> refineWithTrm abs ti m t))

eChoose i = 
  actionIf 
      "nothing to choose" 
      [EAmbiguous] 
      (\st -> let tis = termInfosOfState st 
              in nextEState [tis !!! i] st)

eUndo =
  actionIf 
      "no previous state" 
      [EWork, EAmbiguous, EImpossible, EComplete, EEmpty] 
      (\st -> case st of EState (_:cs) -> EState cs)

eLocalUndo abs i = 
  actionIf 
      "local undo not possible" 
      [EWork, EImpossible, EComplete] 
      (changeTermInfo (\ti -> removeSubTerm abs ti i Nothing))

eLocalWrap abs i fis = 
  actionIf 
      "local undo not possible" 
      [EWork, EImpossible, EComplete] 
      (changeTermInfo (\ti -> removeSubTerm abs ti i (Just fis)))

eWrap abs fti = 
  actionIf 
      "no wrapping possible" 
      [EWork, EComplete] 
      (changeTermInfo (\ti -> wrapTermInfoWithFun abs ti fti))

eTransform abs tc =
  actionIf 
      "no transformation possible until complete" 
      [EComplete] 
      (\st -> case termInfoOfState st of
                Ok ti -> nextEState (mkAllInTI (termCommand abs tc) ti) st
                Bad s -> addMsg s st)

eReset =
  const emptyEState

eActivate meta =
  actionIf
      "no metas in state"
      [EWork]
      (setActiveMeta meta)

eAddConstrs abs cs =
  actionIf
      "impossible to add constraints"
      [EWork, EAmbiguous, EComplete]
      (\st -> if null cs then st else
              let tis0 = termInfosOfState st
                  cs'  = [(compute abs x, compute abs y) | (x,y) <- cs]
                  tis1 = [addConstrsToTInfo ti cs' | ti <- tis0]
                  tis2 = [ti | t <- tis1, Ok ti  <- [derivedRefinements abs t]]
                  tis3 = filter isPossibleTI (map (computeConstrsInTI abs) tis2)
                  tis4 = [ti | Ok ti <- map (checkTermInfo abs) tis3]
              in nextEState tis4 st)

eNew abs cat st =
  tryAddTermInfo (initTermInfo abs cat) (addOptions (optionsOfState st) emptyEState)

eRefines abs mts st =
  foldr (eRefine abs) st mts

eRefineRef abs (m,(trm,typ)) = 
  actionIf 
      "no refinement possible" 
      [EWork] 
      (changeTermInfo (\ti -> refineWithRef abs ti m trm typ))

eRefineAct abs i st = 
  actionIf 
      "no refinement possible" 
      [EWork] 
      (changeTermInfo 
        (\ti -> do
           m  <- firstMetaOfTI ti
           let (trm,typ) = activeRefinements abs st !!! i
           refineWithRef abs ti m trm typ
        )) 
      st

eWrapAct abs i st = 
  eWrap abs (activeWraps abs st !!! i) st

eAmbigRefine abs mts =
  actionIf
      "no refinement possible" 
      [EWork] 
      (\st -> let Ok ti = termInfoOfState st
                  tis = [ti' | (m,t) <- mts, Ok ti' <- [refineWithTrm abs ti m t],
                               isPossibleTI (computeConstrsInTI abs ti')]
                  msg = if null mts then "no result to try refinement with" 
                                    else "all results refuted by type checking"
              in if null tis then addMsg msg st else nextEState tis st)

eDummy = 
  addMsg "dummy action"

eMessage =
  addMsg

-- information on state

-- ePrint :: 

activeRefinements abs st = errVal [] $ refs where
  refs = do
    ti <- termInfoOfState st
    m  <- firstMetaOfTI ti
    return $ refinementsOfGoal abs ti m

activeWraps abs st = errVal [] $ refs where
  refs = do
    ti <- termInfoOfState st
    let ty = typeOfTI ti
    return $ functionsOnType abs ty

-- auxiliaries

changeTermInfo :: (TermInfo -> Err TermInfo) -> Action
changeTermInfo f = 
  actionIf 
    "no unique term info" 
    [EWork, EComplete]
    (\st -> tryAddTermInfo (do
              ti <- termInfoOfState st
              f ti) st)

addMsg :: String -> Action
--- addMsg s (EState ((tis,(msg,opts)):hist)) = EState ((tis, (s:msg,opts)):hist)
--- addMsg s (EState []) = EState [([],([s],[]))]
addMsg s (EState cs) = EState (([],([s],[])):cs) --- more natural in Undo

addOptions :: [Option] -> Action
addOptions os (EState ((tis,(msg,opts)):hist)) = EState ((tis, (msg,os++opts)):hist)
addOptions os (EState []) = EState [([],([],os))]

removeOption :: Option -> Action
removeOption s (EState ((tis,(msg,opts)):hist)) = 
  EState ((tis, (msg,filter (/=s) opts)):hist)
removeOption s st = st

toggleOption o st = if elem o (optionsOfState st) 
                      then removeOption o st else addOptions [o] st

resetOptions :: Option -> Action
resetOptions s (EState ((tis,(msg,_)):hist)) = EState ((tis, (msg,[])):hist)
resetOptions s st = st

nextEState :: [TermInfo] -> Action
nextEState tis s@(EState hist) = EState ((tis,([],optionsOfState s)):hist)

termInfoOfState :: EState -> Err TermInfo
termInfoOfState (EState (([ti],_):_)) = return ti
termInfoOfState _ = Bad "no unique term info"

termOfState st = do
  ti <- termInfoOfState st
  return $ termOfTI ti

metasOfState st = errVal [] $ do
  ti <- termInfoOfState st
  return $ metaSymbsOfTI ti

termInfosOfState (EState ((tis,_):_)) = tis
messageOfState (EState ((_,(m,_)):_)) = m
messageOfState _ = ["empty state"]

optionsOfState (EState ((_,(_,o)):_)) = o
optionsOfState _ = []

setActiveMeta :: MetaSymb -> Action
setActiveMeta m st = tryAddTermInfo (trySetActive st) st where
  trySetActive st = do
    ti <- termInfoOfState st
    m' <- maybeErr "no occurrence of wanted meta" $ lookupMeta m $ metasOfTI ti
    return $ mkFirstMeta ti m' 
  lookupMeta m ms = lookup (nMeta m) [(nMeta m', m') | (m',_) <- ms]
  nMeta (MetaSymb (c,i)) = MetaSymb (zIdent (symid c),i)

riskFirstMetaOfState :: EState -> MetaSymb  -- only works in EWork
riskFirstMetaOfState st = errVal undefined $ do
            ti <- termInfoOfState st
            firstMetaOfTI ti

tryAddTermInfo :: Err TermInfo -> Action
tryAddTermInfo eti = case eti of
  Ok ti -> nextEState [ti]
  Bad s -> addMsg s

actionIf :: String -> [EStatus] -> Action -> Action
actionIf s conds act st = (if elem (eStatus st) conds then act else addMsg s) st

mkIfEStatus :: String -> [EStatus] -> (EState -> a) -> EState -> Err a
mkIfEStatus s conds f st = if elem (eStatus st) conds then Ok (f st) else Bad s

data EStatus = EWork | EAmbiguous | EImpossible | EComplete | EEmpty | EInitial
      deriving Eq

eStatus :: EState -> EStatus
eStatus (EState currents) = case currents of
  ([ti],_):_
    | isImpossibleTI ti -> EImpossible
    | isCompleteTI ti   -> EComplete
    | otherwise         -> EWork
  ([],_):_              -> EEmpty
  _:_                   -> EAmbiguous
  _                     -> EInitial

lengthEState (EState cs) = length cs

