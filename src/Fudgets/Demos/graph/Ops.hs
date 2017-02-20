module Ops(Bop(..),Uop(..),unops,addops,mulops,binoplex,binopfun,unoplex,unopfun,
	show_Bop,show_Uop,addopt,mulopt{-,powopt-})
where
import ListUtil(assoc)
import ListMap(lookupWithDefault)

data Bop = Badd | Bsub | Bmul | Bdiv | Bmin -- | Bpow
           deriving (Eq)

data Uop = Uneg | Usin | Ucos | Utan | Uexp | Ulog | Uln | Usqrt |
	   Uabs | Ufloor | Uceil | Uround | Utanh | Ucosh | Usinh | Ugamma |
	   Uatan | Uacos | Uasin | Usgn | Utheta
		deriving (Eq)

assocdef x xs e = lookupWithDefault xs e x

binoplex binop = fst (assocdef binop binoptab (error "undefined binop"))
binopfun binop = assocdef binop (map snd binoptab) (error "undefined binop")

binoptab = addopfuncs ++ mulopfuncs -- ++ powopfuncs

addopfuncs =
  [("+",(Badd,(+))),
   ("-",(Bsub,(-)))
  ]

mulopfuncs =
  [("*",(Bmul,(*))),
   ("/",(Bdiv,(/))),
   ("!",(Bmin,min))
  ]

{-
powopfuncs =
  [("^",(Bpow,pow))
  ]
-}

addops = map fst addopfuncs
mulops = map fst mulopfuncs
--powops = map fst powopfuncs

addopt = map (fst . snd) addopfuncs
mulopt = map (fst . snd) mulopfuncs
--powopt = map (fst . snd) powopfuncs

unoplex unop = fst (assocdef unop unoptab (error "undefined unop"))
unopfun unop = assocdef unop (map snd unoptab) (error "undefined unop")

-- #ifdef sequent
#if 0
#define F(f) ("f",(U##f,f))
#else
#define F(f) ("f",(U/**/f,f))
#endif

unoptab =
  [("-",(Uneg,negate)),
   F(sin),
   F(cos),
   F(tan),
   F(exp),
   F(log),
   ("ln",(Uln,log)),
   ("log",(Uln,log)),
   F(sqrt),
   ("fabs",(Uabs,abs)),
   ("abs",(Uabs,abs)),
   ("floor",(Ufloor,fromIntegral.floor)),
   ("round",(Uround,fromIntegral.round)),
   --F(ceil),
   F(tanh),
   F(cosh),
   F(sinh),
   --F(gamma),
   F(atan),
   F(acos),
   F(asin),
   F(sgn),
   F(theta)
  ]

theta :: Double->Double
theta x = if x<0.0 then 0.0 else 1.0
sgn x = theta x - theta (0.0-x)

--pow x y = exp (y * log x)
--pow x y = x**y
--pow x y = x^^y

unops = map fst unoptab

show' tab x = assocdef x (map (\(x,(y,_))->(y,x)) tab) (error "show' in Ops.hs")

show_Uop = show' unoptab
show_Bop = show' binoptab
