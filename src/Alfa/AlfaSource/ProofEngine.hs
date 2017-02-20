module ProofEngine where

-- Pick one of the supported proof engines:
--import qualified DummyProofEngine as PE
--import qualified V3ProofEngine as PE -- Interface to V3
import qualified  AgdaProofEngine as PE -- Interface to Agda

import UAbstract(Var,MetaVar,Exp,Decl,Decls) -- Abstract syntax used by Alfa
import EditMonad(EdM,runEd) -- the state/error monad used by Alfa

-- Alfa expects the Proof Engine to provide this interface:


type State = PE.State -- Proof Engine State. Why not call this PEST? :-)
type PE a = EdM State Error a -- Proof Engine Monad
-- PE should be an instance of Monad and Functor
type Error = String -- ??

type PI a = PE a  -- Proof State Inspection
type PQ a = PE a  -- Proof State dependent Query

--type PI a = State -> a -- (would suffice)
--type PQ a = State -> Either Error a -- (would suffice)

type MetaSubst   = [(MetaVar,Exp)] -- how meta variables have been instantiated
type Env         = [(Var,Exp)] -- the types of names in scope
type Constraints = [(Exp,Exp)]


-- The initial Proof Engine State
initState :: State

-- How to execute the proof monad operations:
run :: PE a -> State -> Either Error (a,State)
run = runEd
--- Proof monad operations (may alter the state): ------------------------------

-- Instantiate meta variables:
give :: MetaVar -> Exp -> PE MetaSubst -- returns affected meta variables
appendDecls :: Decls -> PE (Decls,MetaSubst)
--solveConstraint :: Int -> Int -> PE MetaSubst
--
--genMetas :: Int -> PE [MetaVar]
--genMeta :: PE MetaVar

-- Set/get options:
setTerminationCounter :: Int -> PE ()
setAutoSolve :: Bool -> PE ()
getAutoSolve :: PI Bool

--- Proof state inspection (no state changes expected): ------------------------
--topLevelContext :: PI Env
constraints     :: PI Constraints
metaSubst       :: PI MetaSubst -- expected to be idempotent(?)
metaVars        :: PI [MetaVar] -- uninstantiated meta variables
suggestSolution :: PI MetaSubst -- suggested solutions to constraints
checkTermination :: Decls -> PE () -- fails if no termination proof was found

-- Find out how a meta variable can be instantiated
goal    :: Int->MetaVar-> PQ (Env,Exp) --  (context, goal type), Env |- ? : Type
intro   :: MetaVar -> PQ [Exp] -- lists usable "constructors"
refine, refineExact  :: MetaVar -> Exp -> PQ Exp
open, case'   :: MetaVar -> Exp -> PQ Exp
openTLhack :: Exp -> PQ Decl

-- Obtain type or WHNF in the context of a meta var or the top level
typeOf,compute :: Maybe MetaVar -> Exp -> PQ Exp
unfoldN :: Int -> Maybe MetaVar -> Exp -> PE Exp
contUnfold1 :: PE Exp
--------------------------------------------------------------------------------
-- Interface implementation:

initState = PE.initState
give = PE.give
appendDecls = PE.appendDecls
--genMetas = PE.genMetas
--genMeta = PE.genMeta
setTerminationCounter = PE.setTerminationCounter
setAutoSolve = PE.setAutoSolve
getAutoSolve = PE.getAutoSolve
--topLevelContext = PE.topLevelContext
--solveConstraint = PE.solveConstraint
constraints = PE.constraints
metaSubst = PE.metaSubst
metaVars = PE.metaVars
suggestSolution = PE.suggestSolution
checkTermination = PE.checkTermination
goal = PE.goal
intro = PE.intro
refine = PE.refine
refineExact = PE.refineExact
case' = PE.case'
open = PE.open
openTLhack = PE.openTLhack

compute = PE.compute
typeOf = PE.typeOf

unfoldN = PE.unfoldN
contUnfold1 = PE.contUnfold1

--- Derrived operations: -------------------------------------------------------

inspect state pi = 
  case run pi state of
    Right (x,_) -> x

query state pq =
  case run pq state of
    Right (x,_) -> Right x
    Left e -> Left e

changestate state pe =
  case run pe state of
    Right (_,state') -> state'

unfolds state n m e =
  case run (unfoldN n m e) state of
    Right (e',state') -> e' : contUnfolds state'
      where
        contUnfolds state =
	  case run contUnfold1 state of
	    Right (e,state') -> e:contUnfolds state'
	    _ -> []
    _ -> if n==0 then [e] else []
