{-

The proof state and simple read/write operations.  More complex
operations are in ProofMonad.

-}

module ProofState(module ProofState,module MetaVarState) where

import ISyntax(Def,Value,SymTab,initST,ClassEnv,initClassEnv)
import List(delete)
import qualified AltIntMap as I
import MetaVarState
import FString(StrTable)
import Monads
import PreStrings(preStrTable)
import Gensym

-- | DefState is a set of definitions Def, which are either typed, untyped of opens.
type DefState = I.IntMap Def


type TermCounter = Int
defaultCounter :: TermCounter
defaultCounter = 1000

type TransState = (Int,IdSupply)

initTransState :: TransState
initTransState = (1,initIdSupply)

{- | A State is a tuple (defs,ms,ts,t,st,tc,v,xtra).  The different
components are:

(Andreas: I have no clue what these different components are, if you
know, please write the documentation here.)

(Andreas: Added more documentation during Catarinas Talk on the Agda
Implementors Meeting 2004)

defs : DefState 

  -- Agda definitions processed so far.  This is a flat structure, all
  functions are lambda-lifted to the top-level during type checking.
  All identifiers have unique names and are mutually recursive.
  Lambda-lifting solves the problem of local recursive definitions.

ms   : MetaVarState -- see MetaVarState.hs

ts   : TransState -- ?

t    : StrTable -- table of collected strings to make fast strings work

st   : SymTab -- Symbol Table

classEnv :: ClassEnv -- Dictionary of top level classes and instances


tc   : TermCounter -- Counter to not loop

v    : Maybe Value -- Last value in one step reduction.

xtra : XtraState -- ??

-}

data State = State {
    defState :: DefState,
    metaVarState :: MetaVarState,
    transState :: TransState,
    strTable :: StrTable,
    symTab :: SymTab,
    classEnv :: ClassEnv,
    termCounter :: TermCounter,
    calcValue :: Maybe Value, 
    xtraState :: XtraState}

 
-- should be parametraised, at least w.r.t. XtraState



initDefs :: DefState
initDefs = I.empty

initState :: State
initState = State initDefs initMetaVarState initTransState preStrTable initST initClassEnv defaultCounter Nothing initXtra



readDefState :: State -> DefState
readDefState = defState
writeDefState :: DefState -> State -> State
writeDefState defs' state = state {defState = defs'}
accessDefState :: (DefState -> DefState) -> State -> State
accessDefState f s =  writeDefState (f (readDefState s)) s
accessEDefState :: (DefState -> Error DefState) -> State -> Error State
accessEDefState f s = do ds <- f (readDefState s)
                         return (writeDefState ds s)
                     



readMetaVarState :: State -> MetaVarState
readMetaVarState  = metaVarState
writeMetaVarState :: MetaVarState -> State -> State
writeMetaVarState ms' state = state {metaVarState = ms'}
accessMetaVarState :: (MetaVarState -> MetaVarState) -> State -> State
accessMetaVarState f s =  writeMetaVarState (f (readMetaVarState s)) s
accessEMetaVarState :: (MetaVarState -> Error MetaVarState) -> State -> Error State
accessEMetaVarState f s = do ms <- f (readMetaVarState s)
                             return (writeMetaVarState ms s)
                     

readTransState :: State -> TransState
readTransState  = transState
writeTransState :: TransState -> State -> State
writeTransState ts' state = state {transState = ts'} 
accessTransState :: (TransState -> TransState) -> State -> State
accessTransState f s =  writeTransState (f (readTransState s)) s
accessETransState :: (TransState -> Error TransState) -> State -> Error State
accessETransState f s = do ts <- f (readTransState s)
                           return (writeTransState ts s)
                     


readStrTable :: State -> StrTable
readStrTable  = strTable
writeStrTable :: StrTable -> State -> State
writeStrTable t' state = state {strTable = t'}
accessStrTable :: (StrTable -> StrTable) -> State -> State
accessStrTable f s =  writeStrTable (f (readStrTable s)) s
accessEStrTable :: (StrTable -> Error StrTable) -> State -> Error State
accessEStrTable f s = do t <- f (readStrTable s)
                         return (writeStrTable t s)
                     



readSymTab :: State -> SymTab
readSymTab  = symTab
writeSymTab :: SymTab -> State -> State
writeSymTab st' state = state {symTab = st'}
accessSymTab :: (SymTab -> SymTab) -> State -> State
accessSymTab f s =  writeSymTab (f (readSymTab s)) s
accessESymTab :: (SymTab -> Error SymTab) -> State -> Error State
accessESymTab f s = do st <- f (readSymTab s)
                       return (writeSymTab st s)


readClassEnv :: State -> ClassEnv
readClassEnv  = classEnv
writeClassEnv :: ClassEnv -> State -> State
writeClassEnv st' state = state {classEnv = st'}
accessClassEnv :: (ClassEnv -> ClassEnv) -> State -> State
accessClassEnv f s =  writeClassEnv (f (readClassEnv s)) s
accessEClassEnv :: (ClassEnv -> Error ClassEnv) -> State -> Error State
accessEClassEnv f s = do st <- f (readClassEnv s)
                         return (writeClassEnv st s)
  


readTermCounter :: State -> TermCounter
readTermCounter  = termCounter
writeTermCounter :: TermCounter -> State -> State
writeTermCounter tc' state = state {termCounter = tc'}
accessTermCounter :: (TermCounter -> TermCounter) -> State -> State
accessTermCounter f s =  writeTermCounter (f (readTermCounter s)) s
accessETermCounter 
          :: (TermCounter -> Error TermCounter) -> State -> Error State
accessETermCounter f s = do st <- f (readTermCounter s)
                            return (writeTermCounter st s)
                     
                     

readValue :: State -> Maybe Value
readValue  = calcValue
writeValue :: Maybe Value -> State -> State
writeValue v' state = state {calcValue = v'}
accessValue :: (Maybe Value -> Maybe Value) -> State -> State
accessValue f s =  writeValue (f (readValue s)) s
accessEValue 
   :: (Maybe Value -> Error (Maybe Value)) -> State -> Error State
accessEValue f s = do st <- f (readValue s)
                      return (writeValue st s)

type TraceFl = Bool
initTraceFl = False
type XtraState = TraceFl
initXtra = initTraceFl


readXtraState :: State -> XtraState
readXtraState  = xtraState 
writeXtraState :: XtraState -> State -> State
writeXtraState xtra' state = state {xtraState = xtra'}
accessXtraState :: (XtraState -> XtraState) -> State -> State
accessXtraState f s =  writeXtraState (f (readXtraState s)) s
accessEXtraState 
          :: (XtraState -> Error XtraState) -> State -> Error State
accessEXtraState f s = do st <- f (readXtraState s)
                          return (writeXtraState st s)
                     

readTraceFl :: XtraState -> TraceFl
readTraceFl = id
writeTraceFl :: TraceFl -> XtraState -> XtraState
writeTraceFl x _ = x
accessTraceFl :: (TraceFl -> TraceFl) -> XtraState -> XtraState
accessTraceFl = id
accessETraceFl 
          :: (TraceFl -> Error TraceFl) -> XtraState -> Error XtraState
accessETraceFl f s = do st <- f (readTraceFl s)
                        return (writeTraceFl st s)
