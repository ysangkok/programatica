module State where

import Operations
import Parsers
import Tokens
import Grammar
import Macros
import PrGrammar 
import PPrCF
import SymbolTable
import CF
import GFCFIdents
import Update
import IOGrammar
import Tokenize
import Option
import Parsing


type State a = GFState a MoreInfo
type MoreInfo = (Status,[String]) -- keep history here
type Status = Bool -- telling if there already is an abstract grammar ; cf. AGF

emptyState = (emptyPureState,(True,[]))

type GrammarFile = String
type GrammarId = Maybe Language

getLanguage :: State a -> GrammarId -> Language
getLanguage _ (Just lang) = lang
getLanguage st _ = last (allLanguages st) --- make this safe!

{-
instance HasStatus Bool where
 isInitial b = b
 notInitial _ = False

instance HasStatus b => HasStatus (b, a) where
 isInitial (b,_) = isInitial b
 notInitial (b,a) = (notInitial b,a)
-}

type Direction = Maybe String -- file name

-- try all languages and return the first success
anyLangParser :: State Str -> [Option] -> Ident -> String -> ([String],[Trm]) 
anyLangParser st opts cat s = ([], concat $ take 1 $ tryParses langs) where
  langs        = reverse $ allLanguages st --- or select a subset ?
  tryParses ls = filter (not . null) $ map tryOne ls
  tryOne lang  = snd  $ mkOptParser st opts (Just lang) cat s

mkOptParser :: 
  State Str -> [Option] -> GrammarId -> Ident -> String -> ([String],[Trm]) 
mkOptParser st opts gr cat = 
    let lang   = getLanguage st gr
        parser = parserOfLanguage' opts lang st cat
        liter  = snd (cfOfLanguage lang st) . readTok
        tokzer = tokenizerOfLanguage lang st
    in mkOptParserOne opts parser liter tokzer


-- fonts for Fudget sessions; this is not the proper place ---

myUniFont, myAscFont :: FontId
myUniFont = "-mutt-clearlyu-medium-r-normal--0-0-100-100-p-0-iso10646-1"
myAscFont = --- "lucidasanstypewriter-12"
            --- "-adobe-courier-bold-r-normal--18-180-75-75-m-110-iso8859-1"
            "-adobe-courier-medium-r-normal--17-120-100-100-m-100-iso8859-1"
