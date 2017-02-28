{-# LANGUAGE NoMonomorphismRestriction #-}
module Deriving where
import DerivingUtils
import PrettyPrint hiding (con, var)
import Lift(lift)
import SrcLoc(srcLoc)
import Data.Maybe(mapMaybe)

derive stdnames cl t =
    case lookup cl derivers of
      Nothing -> fail $ pp $"Don't know how to derive"<+>cl
      Just d -> d stdnames' (srcLoc cl) =<< tinfo
  where
    stdnames' m n = lift (stdnames m n)
    tinfo =
      case idTy t of
	Type tinfo@TypeInfo{defType=Just tkind}
           | tkind `elem` [Data,Newtype] -> return (idName t,tinfo)
	_ -> fail $ pp $ "Deriving"<+>cl<>": this is not a data/newtype:"<+>t
{-
    conv tinfo@(TypeInfo d cs fs) = TypeInfo d (map convc cs) [] -- !!fields
      where
        convc (ConInfo c n optfs) =
          ConInfo (PNT (mkUnqual c) ConstrOf{} noSrcLoc)
		  n
		  Nothing -- !! field names are discarded
-}
    derivers =
	concat
	[pc "Eq"  deriveEq,
	 pc "Show" deriveShow,
	 pc "Bounded" deriveBounded,
	 pc "Enum" deriveEnum,
	 pc "Ord" deriveOrd,
	 pc "Read" deriveRead,
	 ixc "Ix" deriveIx]

    pc = stdc mod_Prelude
    ixc = stdc mod_Ix
    stdc m n d = either ignore keep (stdclass stdnames m n)
      where
        keep (HsCon c) = [(c,d)]
	ignore _ = []

deriveBounded stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
     HsVar minBound <- pv "minBound"
     HsVar maxBound <- pv "maxBound"
     let enumBounds cs@(c:_) = [b minBound c,b maxBound (last cs)]
	 b m ci = fun0 src m (con c)
	   where
	     c=convCon t (conName ci)

	 tupleBounds [ConInfo{conName=c0,conArity=n}] =
	     [tb minBound,tb maxBound]
	   where
	     tb m = fun0 src m (apps (con c:replicate n (var m)))
	     c=convCon t c0
     if isEnum cs
       then return (enumBounds cs)
       else if length cs == 1
            then return (tupleBounds cs)
            else fail "Deriving Bounded: the type is neither an enumeration nor a single constructor type."

deriveEnum stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
     HsVar fromEnum <- pv "fromEnum"
     HsVar toEnum <- pv "toEnum"
     if isEnum cs
       then return (fromToEnum src fromEnum toEnum t id)
       else fail "Deriving Enum: the type is not an enumeration."

fromToEnum src fromEnum toEnum t@(_,TypeInfo{constructors=cs}) sig =
   [fromEnumDef,toEnumDef]
 where
  cns = zip [0..] cs

  fromEnumDef = fun src (map fromAlt cns)
  toEnumDef = fun src (map toAlt cns)

  fromAlt (n,ci) = alt1 src fromEnum (con c)
			((if n==0 then sig else id) (hsLit src (HsInt n)))
    where c = convCon t (conName ci)

  toAlt (n,ci) = alt1 src toEnum (hsPLit src (HsInt n)) (con c)
    where c = convCon t (conName ci)

deriveEq stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
     HsVar eq <- pv "=="
     true <- pv "True"
     false <- pv "False"
     andand <- pv "&&"
     let def = if length cs>1
	       then [alt2 src eq wild wild (ident false)]
	       else []
	 eqalt ConInfo{conName=c0,conArity=n} =
	    alt2 (srcLoc c0) eq (p xs) (p ys) rhs
	   where
	     c = convCon t c0
	     p vs = hsPApp c vs
	     rhs = conj andand true comps
	     comps = zipWith eqtst xs ys
	     eqtst = opapp (HsVar eq)
	     xs=take n (vars "x")
	     ys=take n (vars "y")

     return [fun src (map eqalt cs++def)]

{-+
When deriving #Ix# for an enumerations, the three operations (#range#, #index#,
 #inRange#) can easily be defined assuming you have auxiliary functions to
convert to and from Int. If there is a derived Enum instance,
toEnum and fromEnum could be used, but since we don't know if there is an
Enum instance, we define the same functions locally in each method definition.
This produces some code duplication. Grr!

Alternatively, the #inRange# method could be defined using (<=) from
the Ord class. We know that there is an Ord instance, since Ord is a
superclass of Ix, but we don't know if it is the derived Ord. A user
defined Ord instance can not be used. Grr again!
-}

deriveIx stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
         pt = stdtype stdnames mod_Prelude
	 ixv = stdvalue stdnames mod_Ix
     int <- hsTyId # pt "Int"
     HsVar range <- ixv "range"
     HsVar index <- ixv "index"
     HsVar inRange <- ixv "inRange"
     rangeSize <- ixv "rangeSize"

     fromEnum@(HsVar fromE) <- pv "fromEnum"
     toEnum@(HsVar toE) <- pv "toEnum"
     true <- pv "True"
     andand <- pv "&&"
     map <- pv "map"
     plus  <- opapp # pv "+"
     times <- opapp # pv "*"
     let enumIx cs@(c:_) = [erange,eindex,einRange]
	   where
	     erange = pfun' range (l,u)
			    (mapE (ident toEnum)
				(apps [var range,pair (fromEnumE l) (fromEnumE u)]))
			    (toDefs fromto)

	     eindex = pfun2' index (l,u) i
			     (apps [var index,pair (fromEnumE l) (fromEnumE u),
					      fromEnumE i])
			     (oneDef from)

	     einRange = pfun2' inRange (l,u) i
			     (apps [var inRange,pair (fromEnumE l) (fromEnumE u),
						fromEnumE i])
			     (oneDef from)

	     fromto@[from,to] = fromToEnum src fromE toE t ( -:: int)

	     l = var (localVal "l")
	     u = var (localVal "u")
	     i = var (localVal "i")

	     fromEnumE = app (ident fromEnum)

	 -- pre: n>=1
	 tupleIx [ConInfo{conName=c0,conArity=n}] = [trange,tindex,tinRange]
	    where
	     trange = pfun range rpat $ hsListComp (foldr gen last (zip [1..] luis))
	       where
		 gen (n,(l,u,i)) =
		   HsGenerator (fakePos src n) i (var range `app` pair l u)
		 last = HsLast (apps (con c:is))

	     tindex = ilhs index $
		      case luis of
		       [lui] -> ix1 lui
		       _ -> foldl ix zero luis
	       where
		 ix1 (l,u,i) = apps [var index,pair l u,i]
		 ix acc (l,u,i) = ix1 (l,u,i) `plus`
				  ((ident rangeSize `app` pair l u) `times` acc)

	     tinRange = ilhs inRange $
			conj andand true [apps [var inRange,pair l u,i]|(l,u,i)<-luis]

	     ilhs f = pfun2 f rpat (hsPApp c is)
	     rpat = (hsPApp c ls,hsPApp c us)

	     c = convCon t c0
	     luis = zip3 ls us is
	     ls = take n (vars "l")
	     us = take n (vars "u")
	     is = take n (vars "i")

	 pfun f p rhs = pfun' f p rhs noDef
	 pfun' f (x,y) rhs ds = fun src [alt1' src f (pair x y) rhs ds]
	 pfun2 f p z rhs = pfun2' f p z rhs noDef
	 pfun2' f (x,y) z rhs ds = fun src [alt2' src f (pair x y) z rhs ds]

	 zero  = hsLit src (HsInt 0)

	 mapE f xs = apps [ident map,f,xs]

     if isEnum cs
       then return (enumIx cs)
       else if length cs == 1
            then return (tupleIx cs)
            else fail "Deriving Ix: the type is neither an enumeration nor a single constructor type."

deriveOrd stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
         pt = stdtype stdnames mod_Prelude
     HsVar compare <- pv "compare"
     lexOrder <- pv "lexOrder" -- nonstandard entity!
     int <- hsTyId # pt "Int"
     let branches = mapMaybe eqbranch cs `asTypeOf` def
	 n = length cs
	 def = if n<2 && length branches==n
	       then []
	       else [alt x y cmpcno]
	   where
	     x = var (localVal "x")
	     y = var (localVal "y")
	     cmpcno = hsLet cnodef (compareE (cnoE x) (cnoE y))
	     cnodef = oneDef (fun src (zipWith cnoalt [0..] cs))
	     cno = localVal "cno"
	     cnoE a = var cno `app` a
	     cnoalt i ConInfo{conName=c0,conArity=n} = 
		  alt1 src cno (hsPApp c xs) (l i)
		where xs = replicate n wild
		      c = convCon t c0
	     l 0 = intlit 0 -:: int -- add type restriction to the first branch
	     l i = intlit i
	     intlit = hsLit src . HsInt
	 eqbranch ConInfo{conName=c0,conArity=n} =
	     if n==0
	     then Nothing
	     else Just (alt (p xs) (p ys) rhs)
	   where
	     c = convCon t c0
	     p = hsPApp c
	     rhs = conj comps
	     conj = foldr1 (opapp lexOrder)
	     comps = zipWith compareE xs ys
	     xs = take n (vars "x")
	     ys = take n (vars "y")

	 compareE = opapp (HsVar compare)

	 alt = alt2 src compare

     return [fun src (branches++def)]

deriveRead stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
     readParenArg <- pv "readParenArg"
     readArgument <- pv "readArgument"
     readToken <- pv "readToken"
     HsVar readsPrec <- pv "readsPrec"
     readAp <- pv "readAp"
     readChoice <- pv "readChoice"

     let d = var (localVal "d")

	 alt = alt1 src readsPrec d

	 rdCon ConInfo{conName=c0,conArity=n} =
	   case n of
	     0 -> rdConName cn c
	     _ -> rdParenArg (comp (rdConName cn c:replicate n rdArg))
	   where
	     c = convCon t c0
	     cn = getBaseName c0

	 rdConName cn c = rdToken (con c) (str src cn)
	 rdToken = opapp readToken
	 rdParenArg a = opapp readParenArg d a
	 rdArg = ident readArgument

	 comp = foldl1 (opapp readAp)
	 choice = foldr1 (opapp readChoice)


     return [fun src [alt (choice (map rdCon cs))]]

deriveShow stdnames src t@(_,TypeInfo{constructors=cs}) =
  do let pv = stdvalue stdnames mod_Prelude
     showParenArg <- pv "showParenArg"
     showArgument <- pv "showArgument"
     showString <- pv "showString"
     HsVar showsPrec <- pv "showsPrec"
     comp2 <- opapp # pv "."

     let showAlt ConInfo{conName=c0,conArity=n} =
	   case n of
	     0 -> alt (con c) (showConName cn)
	     _ -> alt (hsPApp c xs)
		      (paren (showConName cn `comp2` comp (map showArg xs)))
	   where
	     c = convCon t c0
	     cn = getBaseName c0
	     xs = take n (vars "x")

	 paren arg = ident showParenArg `app` d `app` arg
	 showArg v = ident showArgument `app` v
	 showConName c = ident showString `app` str src c

	 comp = foldr1 comp2

	 alt = alt2 src showsPrec d
	 d = var (localVal "d")

     return [fun src (map showAlt cs)]
