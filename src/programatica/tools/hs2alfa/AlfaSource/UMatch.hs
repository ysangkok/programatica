module UMatch(Pat(..),Rule,Rules,TypeInfo,match,exhaustiveMatch,exhaustiveMatch',exhaustiveMatch'',exhaustiveMatchInCtx) where
import UAbstract(Con,Var(..),Exp(..),Branch(..),Typing(..),absExp,ePreMetaVar)
import USubstitute(substitute,freeIn)
--import UFree
import Utils2(mapFst)
import Data.List(sortBy)
--import Debug2(badtrace)


-- A type for nested patterns:
data Pat
  = PVar Var
  | PCon Con [Pat]
  | PAs Var Pat
  deriving (Show)

-- A rule is one equation in a function definition, excluding the function name
type Rule = ([Pat],Exp) -- f p_1 .. p_n = e, excluding f
type Rules = [Rule] -- all rules must have the same number of patterns

-- To construct exhaustive case expressions, we need for each relevant type
-- a list of its constructors and their arities.
type TypeInfo = [[(Con,Int)]]

-- The arguments are the rules to simplify and the expression to use in
-- the cases when no equation matches.
match :: Rules -> Exp -> Exp
match = exhaustiveMatch []

exhaustiveMatch :: TypeInfo -> Rules -> Exp -> Exp
exhaustiveMatch ts = exhaustiveMatch' ts (repeat ePreMetaVar)

exhaustiveMatchInCtx :: TypeInfo -> [Var] -> Rules -> Exp -> Exp
exhaustiveMatchInCtx ts params rules def =
{-
    badtrace (unwords ["exthmInCtx",show ts,show params,show rules,show def,"=",
		       show res]) $
    res
  where
   res =-} snd (exhaustiveMatch'' ts params rules def)

exhaustiveMatch' :: TypeInfo -> [Exp] -> Rules -> Exp -> Exp
exhaustiveMatch' ts argtypes rules def =
    absExp (zipWith (:-) us argtypes) e
  where
    (us,e) = exhaustiveMatch'' ts [] rules def

exhaustiveMatch'' :: TypeInfo -> [Var] -> Rules -> Exp -> ([Var],Exp)
exhaustiveMatch'' ts params rules def = 
    (us,match' us' us rules def)
  where
    arity = length (fst (head rules))
    (us,us') = splitAt arity (params++varnames)
    varnames = [v | n <- [1..],
		let v=Var ("u"++show n),
		isUnused v]
    --vs = free (def,rhss)
    --isUnused v = v `notElem` vs
    --{-
    isUnused v@(Var s) = not (v `freeIn` def || any (v `freeIn`) rhss)
			 || length s>3 -- assume u100 and above are unused!!!
    --}
    rhss = map snd rules

-- The rest is internal stuff, copied and adapted from my compiler Fl2 ---------

-- match below is an implementation of the function match described
-- in section 5.2 of S P Jones' book: The Implementation of Func. Prog. Lang.
    match' ns [] qs default' = theEmptyRule ns qs default'
    match' ns us qs default' =
	-- Assume the mixture rule (5.2.6) is required, since
	-- the variable rule & the constructor rule are just special
	-- cases of the mixture rule.
	theMixtureRule ns us (partition'' qs) default'

    -- match2 assumes that the mixture rule has already been applied
    match2 ns [] qs default' = theEmptyRule ns qs default'
    match2 ns (u : us) qs default' =
	if firstPatIsVar (head qs) then
	    theVariableRule ns u us qs default'
	else
	    theConstructorRule ns u us (collect qs) default'

    theVariableRule ns u us qs default' =
	match' ns us (map (fixGuards default' . applyRuleTo (EVar u)) qs) default'

    -- 5.2.2
    theConstructorRule ns u us qqs default' =
	-- Duplicating the default' exp can cause code explosion.
	-- Using a Let to share it loses some strictness.
	let v = EVar u
	    c d ns' = ehCase ts ns v (map (sameConstructor ns' v us d) qqs) d
	in --if issimple default' -- || "expcase" `elem` options
	   --then
	   c default' ns
	   {-
	   else let defv:ns'' = ns
		    defe=EVar defv
		in letVarE defv default' (c defe ns'') -}


    -- 5.2.3
    theEmptyRule ns qs default' =
	case qs of
	  [] -> default'
	  -- 5.2.4
	  ([], e) : _ -> e
	  _ -> error $ "UMatch.theEmptyRule "++show qs -- synax error

    theMixtureRule ns us [qs] default' = match2 ns us qs default'
    theMixtureRule ns us (qs : qss) default' =
	match2 ns us qs (theMixtureRule ns us qss default')
	-- ns twice?!

    sameConstructor ns u us default' qs =
	let (ar, c) =
		case (rmAsP . head . fst . head) qs of
		  PCon cn ps -> (length ps, cn)
		  --NumP n -> (0, const (NumP n))
		  --CharP n -> (0, const (CharP n))
	    (vs,ns') = splitAt ar ns
	    --varpats = map PVar vs
	    stripConstructor (PCon _ ps : rs, e) = (ps ++ rs, e)
	    stripConstructor (PAs x p : rs, e) =
	       stripConstructor (p:rs, substitute u x e)
	    stripConstructor (_ : rs, e) = (rs, e) -- NumP or CharP
	in  Branch (c,(vs,
	     match' ns' (vs ++ us) (map (fixGuards default' . stripConstructor) qs) default'))

applyRuleTo u (PAs x p : ps, e) = applyRuleTo u (p:ps, substitute u x e)
applyRuleTo u (PVar x : ps, e) = (ps, substitute u x e)

-- collect: bring patterns with the same leftmost constructor together
collect qs =
    let cid (PCon cn _) = cn
        --cid (NumP n) = NumCon n
        --cid (CharP c) = CharCon c
	cid p = error ("UMatch.cid "++show p)
        cid' = cid.rmAsP.head.fst
    in  partition' cid' (sortBy (\x -> \y -> compare (cid' x) (cid' y)) qs)

partition'' = {-concatMap partitionGuard .-} partition' firstPatIsVar

partition' p [] = []
partition' p qs@(q : _) =
    let (qs1, qs') = span (\r -> p r == p q) qs
    in  qs1 : partition' p qs'

leftmostcolumn xs = map (head . fst) xs

remainingcolums xs = mapFst tail xs

firstPatIsVar (p : _, _) = patIsVar (rmAsP p)

patIsVar (PVar _) = True
patIsVar _ = False

rmAsP (PAs _ p) = rmAsP p
rmAsP p = p

no_match = ePreMetaVar -- Fail "NO_MATCH"

fixGuards def (ps,e) = (ps,e)

-- Exhaustive case:
ehCase :: TypeInfo -> [Var] -> Exp -> [Branch] -> Exp -> Exp
ehCase ts us v bs def = ECase v (bs++bs2)
  where
    cs = [c | Branch (c,_) <- bs]
    allcs = case [t | t <- ts,any (`elem` map fst t) cs] of
              --[allcs] -> allcs
              allcs:_ -> allcs -- tolerate duplicates...
	      _ -> []
    cs2 = filter ((`notElem` cs) . fst) allcs
    bs2 = [Branch (c,(take n us,def)) | (c,n) <- cs2]
