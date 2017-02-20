module CmdLineEnv(options, progName, resourceName, args, argKey, argReadKey, argFlag, argKeyList) where
import System.IOUtil(progArgs,progName,getEnvi)
import FilePaths(aFilePath,pathTail)
import Data.ListUtil(chopList,breakAt)
import HO(apFst, apSnd)
import Data.Char
import Data.Maybe(fromMaybe)
--import NonStdTrace(trace)

argReadKey key def = case lookupOptions key of
	   Nothing -> def
	   Just a -> case reads a of
		(v,_):_ -> v
		_ -> error (" Illegal value to flag -"++key++
			    " (default value is "++show def++" of type "++
			    showType def++"): "++a)
#ifndef __HBC__
  where showType _ = "<type??>"
#endif

argKey key def = lookupOptions key `elseM` def
argFlag key def = argKey key (if def then yes else no) == yes
argKeyList key def = maybe def (chopList (breakAt ':')) (lookupOptions key)

lookupOptions key = lookup key options
	      `orM` (env ("FUD_"++ removePath progName++"_"++key))
	      `orM` (env ("FUD_"++key))

    where removePath = reverse . fst . breakAt '/' . reverse
	  env e = getEnvi e >>= \v -> Just (if v == "" then yes else v)
	  --getEnvi' e = trace ("getEnvi "++e) $ getEnvi e

orM :: Maybe a -> Maybe a -> Maybe a
--orM = (++)
orM Nothing  b = b
orM a        b = a

elseM :: Maybe a -> a -> a
elseM = flip fromMaybe
--elseM (Just a) a' = a
--elseM Nothing  a' = a'

yes = "yes"
no = "no"

(args, options) =
    let parse (('-' : ak) : av : al) = case av of
	      '-':avr -> if not (null avr) && isAlpha (head avr)
	          && reverse (take 4 (reverse ak)) /= "font" 
		  then apSnd ((ak, yes) :) (parse (av:al))
		  else thearg
	      _ -> thearg
            where thearg = apSnd ((ak, av) :) (parse al)
        parse ['-' : ak] = ([], [(ak, yes)])
        parse ("-":al) = (al,[])
        parse (a : al) = apFst (a :) (parse al)
        parse [] = ([], [])
    in  parse progArgs


resourceName = pathTail (aFilePath progName)
