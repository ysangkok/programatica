{-

-}

module MetaVarState  where
import ISyntax
import CITrans
import Gensym
import PPrint 
import Utilities (pp,t)
import Position
import qualified AltIntMap as I
import Maybe(mapMaybe)
-- Lägg till position till meta variabler!!

--Binds a name to it's canonical type



-- Definition concerning only meta variables


data  MetaVarV = MetaVarV {
                pos :: Position,
                parseInfo :: Bool,
                transInfo :: TransClass,
                prec :: Int,  -- precendence for printing when replacing ? by its solution.  Depending on the context in which ? appears, its solution must be enclosed in parentheses.
                visibility :: Visibility,
                context :: Context,
                environment :: Environment,
                metaJudg :: Judgement MetaVar
                }

type MetaEnv = I.IntMap MetaVarV

-- | An equality constraint |v = v|.
data Constraint = Constraint Value Value

instance PPrint Constraint where
     pPrint d _  (Constraint v1 v2) = separate [pp d v1,nest 1 (t"= ") ,pp d v2]

{-
data TypingConstraint = TypingConstraint MetaVar TCEnv [UId] LetDef

instance PPrint TypingConstraint where
     pPrint d _  (TypingConstraint _ _ def) = pp d def
-}
-- Meta variables are substituted for type correct expressions
type MetaSubst = (Bool,MetaVar,Exp)

-- | This is a partial map from metavariables to expressions.
type MetaSubsts  = I.IntMap Exp
{-
getMetaVarVContext :: MetaVarV -> Context
getMetaVarVContext (_,_,_,_,_,gamma,_,_) = gamma
getMetaVarVEnv :: MetaVarV -> Environment
getMetaVarVEnv (_,_,_,_,_,_,env,_) = env
getMetaVarVJudg :: MetaVarV -> Judgement MetaVar
getMetaVarVJudg (_,_,_,_,_,_,_,j) = j
getMetaVarVPosition :: MetaVarV -> Position
getMetaVarVPosition (p,_,_,_,_,_,_,_) = p
getMetaVarVCIT ::  MetaVarV ->  TransClass
getMetaVarVCIT (_,_,cit,_,_,_,_,_) = cit
--getMetaVarVTrace (_,_,_,_,ts,_) = ts
getMetaVarVPI :: MetaVarV -> Int
getMetaVarVPI (_,_,_,pi,_,_,_,_) = pi
getMetaVarVParse:: MetaVarV -> Bool
getMetaVarVParse (_,pai,_,_,_,_,_,_) = pai

getMetaVarVAutomatic :: MetaVarV ->  Visibility
getMetaVarVAutomatic (_,_,_,_,automatic,_,_,_) = automatic
-}

-- The components of the meta var state is:
-- 1. MetaEnv, a map from meta-avriables to MetaVarV (see above)
-- 2. [Constraint], a list of equality constrints
-- 3. [TypingConstraint], a list of typing constraints (not implemented yest)
-- 4. MetaSubst which binds a meta variable to the expression that it
-- has been substituted for.
-- 5. [MetaVar] which is the list of meta vars that has not yet been bound
-- 6. [MetaVar] which is the list of "recent" meta vars that has been automatically bound,
-- All meta variables must be in (1). All meta variables must be either
-- in the domain of (4) or in (5). The domain of (4) and (5) are "disjoint".
-- (6) is a "subset" of the domain of (4)


data MetaVarState = MetaVarState {
        mEnv :: MetaEnv,
        constrs :: [Constraint],
        -- tconstrs :: [TypingConstraint],
        metaSubsts :: MetaSubsts,
        unboundMetas :: [MetaVar],
        boundMetas :: [MetaVar]
   }

initMetaVarState :: MetaVarState
initMetaVarState = MetaVarState I.empty [] I.empty [] []

readMetaEnv :: MetaVarState -> MetaEnv
readMetaEnv = mEnv
writeMetaEnv :: MetaEnv -> MetaVarState -> MetaVarState
writeMetaEnv mEnv' mvs = mvs {mEnv = mEnv'}
accessMetaEnv :: (MetaEnv -> MetaEnv) -> MetaVarState -> MetaVarState
accessMetaEnv f ps = writeMetaEnv (f (readMetaEnv ps)) ps

readConstraints :: MetaVarState -> [Constraint]
readConstraints = constrs
writeConstraints :: [Constraint] -> MetaVarState -> MetaVarState
writeConstraints cs mvs = mvs {constrs = cs}
accessConstraints :: ([Constraint] -> [Constraint]) -> MetaVarState -> MetaVarState
accessConstraints f ps = writeConstraints (f (readConstraints ps)) ps

{-
readTypingConstraints :: MetaVarState -> [TypingConstraint]
readTypingConstraints = tconstrs
writeTypingConstraints :: [TypingConstraint] -> MetaVarState -> MetaVarState
writeTypingConstraints tcs mvs = mvs {tconstrs = tcs}
accessTypingConstraints :: ([TypingConstraint] -> [TypingConstraint]) -> MetaVarState -> MetaVarState
accessTypingConstraints f ps = writeTypingConstraints (f (readTypingConstraints ps)) ps
-}


readMetaSubst ::  MetaVarState -> MetaSubsts 
readMetaSubst = metaSubsts
writeMetaSubst :: MetaSubsts -> MetaVarState -> MetaVarState
writeMetaSubst msubsts mvs = mvs {metaSubsts = msubsts}
accessMetaSubst :: (MetaSubsts -> MetaSubsts) -> MetaVarState -> MetaVarState
accessMetaSubst f ps = writeMetaSubst (f (readMetaSubst ps)) ps


readUnboundMetaVars ::  MetaVarState -> [MetaVar]
readUnboundMetaVars  = unboundMetas
writeUnboundMetaVars :: [MetaVar] -> MetaVarState -> MetaVarState
writeUnboundMetaVars ms mvs = mvs {unboundMetas = ms}
accessUnboundMetaVars :: ([MetaVar] -> [MetaVar]) -> MetaVarState -> MetaVarState
accessUnboundMetaVars f ps = writeUnboundMetaVars (f (readUnboundMetaVars ps)) ps


readBoundMetaVars ::  MetaVarState -> [MetaVar]
readBoundMetaVars  = boundMetas
writeBoundMetaVars :: [MetaVar] -> MetaVarState -> MetaVarState
writeBoundMetaVars ms mvs = mvs {boundMetas = ms}
accessBoundMetaVars :: ([MetaVar] -> [MetaVar]) -> MetaVarState -> MetaVarState
accessBoundMetaVars f ps = writeBoundMetaVars (f (readBoundMetaVars ps)) ps



{-
markAut :: MetaVar -> MetaVarState -> MetaVarState
markAut m mvs = accessMetaEnv markAutInEnv mvs
     where markAutInEnv :: MetaEnv -> MetaEnv
           markAutInEnv menv = 
                 case I.ilookup m menv of
                                Nothing -> error "markAut"
                                Just (pos,pai,cit,pi,hidden,g,t,j) -> I.add (m,(pos,pai,cit,pi,fmap (\x -> True) hidden,g,t,j)) 

 -} 
