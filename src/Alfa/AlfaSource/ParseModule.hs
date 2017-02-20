module ParseModule where
import Fud(aFilePath,pathTail)
import qualified FileConv as FC
import AlfaPlugin
import Plugins
import AlfEditMonad
import EditMonad
import AlfState(pluginState)
import TextFileConv(fromFile)
import List(isPrefixOf)

parseModule imported filename str =
    do plugins <- pluginState `fmap` loadEd
       optm <- if ext `elem` ["","alfa","agda"]
	       then return (alfaParser str)
	       else tryParsers plugins
       case optm of
         Right m -> return m
	 Left e -> errorEd e
  where
    ext = extension filename

    alfaParser = alfaParser' . fromFile
    alfaParser' str = if "Module [" `isPrefixOf` str
                      then case reads str of
			     [(m,r)] | lex r==[("","")] -> Right m
			     _ -> Left "read::String->Module parse error"
                      else FC.parseModule filename str

    tryParsers [] = return (alfaParser str)
    tryParsers (p:ps) =
      tryPluginParser p `handleEd` const (tryParsers ps)

    tryPluginParser (name,Plugin st m) =
        do parse <- liftMaybeEd err (lookup ext (fileparsers m))
	   case parse st imported filename str of
	     Right (mod,st') ->
	       do updatePluginEd (name,Plugin st' m)
	          return (Right mod)
	     Left (pos,err) -> return (Left (show pos++": "++err))
      where
        err = name++" has no parser for "++ext++" files"


-- Extract the extension of a file name, i.e., the part after the last dot.
extension = ext . pathTail . aFilePath
  where
     ext s = if '.' `elem` s
             then reverse . takeWhile (/='.') . reverse $ s
	     else ""
