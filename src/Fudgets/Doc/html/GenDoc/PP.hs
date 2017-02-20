module PP where
import Syntax
import HtmlUtils
import Utils(mix)
import Char(toLower)
import NonStdTrace(trace)

{- This is a pretty printer that generates HTML from the abstract syntax.

   The first argument (es) to these function is a pair

	   (this,links) :: (String,[(String,String)]),

   where `this' is the URL of the web document being generated and links is
   a lookup table giving URLs to web page containing the definition of a
   name (used in prName). If you don't want any hyperlinks you can
   pass ("",[]),
-}

undef n x = trace (n++" documented but undefined.") x

ppValueDef es (n,Nothing) = undef n [n++" :: ??"]
ppValueDef es (_,Just d) = [ppSig es d] -- ok

ppTypeDef es (n,Nothing) = undef n [unwords [strong "type",n,"= ??"]]
ppTypeDef es (_,Just d) =
  case d of
    Data d (Just cs) ->
        [unwords [strong "data",ppCon es ppVar d,"=",
         mix (map ppCCon cs) " | "]]
      where
	ppCCon (ctx,constr) = ppOptCtx es ctx ++ ppConstr constr
	ppConstr constr =
	  case constr of
	    PlainCon t ->  ppCon ("",[]) (ppAType es) t
	    LabelledCon con lbls ->
	        unwords [ppName ("",[]) con,"{", mix (map ppLbl lbls) ", ", "}"]
	      where
		ppLbl (names,t) =
		   unwords [mix (map (ppName es) names) ", ","::",ppType es t]
    Class d ms ->
        unwords [strong "class",ppCon es ppVar d,strong "where"]:
	map (("   "++).ppSig es) ms
    _ -> ppTopDecl es d

ppTopDecl es dec =
    case dec of
      Data d Nothing -> [unwords [strong "data",ppC d]]
      Data d _ -> [unwords [strong "data",ppC d,"= ..."]]
      Type d t -> [unwords [strong "type",ppC d,"=",ppType es t]]
      Class d _ -> [unwords [strong "class",ppC d,strong "where","..."]]
      Instance [] d -> [unwords [strong "  instance",ppCon es (ppAType es) d]]
      Instance c d -> [unwords [strong "  instance",ppCt c,"=>",ppCon es (ppAType es) d]]
      Value sig -> [ppSig es sig]
      Infix f i n ->
        [unwords [strong ("infix"++map toLower (show f)),show i,ppName es n]]
  where
   ppC = ppCon es ppVar
   ppCt = ppCtx es ppVar

ppSig es (Sig f t) = ppName es f ++ " :: " ++ ppCType es t

ppName (this,es) arg = 
  case lookup arg es of
    Just f | f/=this -> on ("A HREF="++html f)++escape arg++off "A"
    _ -> escape arg -- on ("A NAME="++html arg)++escape arg++off "A"

ppVar v = wrap "VAR" (escape v)

ppCType es (CT ctx t) = ppOptCtx es ctx ++ ppType es t

ppOptCtx es ctx =
    case ctx of
      [] -> ""
      _ -> ppCtx es ppVar ctx ++ (escape " => ")

ppCtx es ppArg ctx = "(" ++ mix (map (ppCon es ppArg) ctx) ", " ++ ")"

ppType es t =
  case t of
    ConT (Fun t1 t2) -> ppBType es t1 ++ escape " -> " ++ ppType es t2
    _ -> ppBType es t

ppBType es t =
  case t of
    ConT ct@(Con _ _) -> ppCon es (ppAType es) ct
    AppT a ts -> unwords (ppVar a : map (ppAType es) ts)
    _ -> ppAType es t

ppAType es t =
  case t of
    VarT a -> ppVar a
    ConT ct@(List _) -> ppCon es (ppType es) ct
    ConT ct@(Tuple _) -> ppCon es (ppType es) ct
    ConT ct@(Con _ []) -> ppCon es (ppType es) ct
    t -> "("++ppType es t++")"

ppCon es ppArg c =
  case c of
    Con c as -> unwords (ppName es c:map ppArg as)
    List a -> "["++ppArg a++"]"
    Fun a b -> ppArg a ++ "->" ++ ppArg b
    Tuple as -> "("++mix (map ppArg as) ", "++")"
