{-# COMPILERFLAGS -fno-overload-restr #-}
module HtmlFormHandler(formHandlerSP) where
import Fudgets
import HtmlFormF2(FormMsg(..))
import qualified OrdMap as T
import Maybe(fromMaybe)

#ifdef __GLASGOW_HASKELL__
formHandlerSP :: (Ord a, Ord b) => SP (Maybe (a, (b, FormMsg))) (Either (b, FormMsg) (a, [(String, String)]))
#endif
formHandlerSP = mapstateSP formHandler (Nothing,T.empty)

state0 = (Nothing,T.empty)

formHandler _ Nothing = (state0,[])
formHandler state@(waitingfor,forms) (Just (formpath,(n,msg))) =
    case msg of
      Reset -> (state,toAll msg)
      Submit -> ((Just elems,forms),toAll msg)
      RadioChange (n,v) -> (state,toAll msg)
      Output values ->
	  case waitingfor of
	    Nothing -> ((waitingfor,forms'),[])
	    Just es -> 
		case es' of
		  [] -> ((Nothing,forms),
			 [Right (formpath,concat (T.elems form'))])
	          _ -> ((Just es',forms'),[])
	      where es' = [ e |e<-es,e/=n]
	where
	  form' = T.add (n,values) form
	  forms' = T.add (formpath,form') forms

  where
    form = fromMaybe T.empty (T.lookup formpath forms)
    toAll msg =[Left (n,msg) | n<-elems]
    elems = T.indices form

--tr x = ctrace "formHandler" x x
