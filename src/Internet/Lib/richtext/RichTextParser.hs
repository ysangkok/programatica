module RichTextParser(parseRichText,Error(..)) where
import RichText
import RichTextLex
import ParsOps
import ListUtil(assoc)
import Data.Char(toLower)

{- BNF for richtext:

richtext ::= { item }
item     ::= chars
	     charfmtcmd
	     fmtpara

fmtpara ::= parafmtcmd richtext /parafmtcmd

-}

parseRichText s =
   case parse richtext (rtlex s) of
     Right rt -> Right rt
     Left (expected,ts) -> Left (expected, showRtls ts)

richtext = many item

item = charfmtcmd `orelse` fmtpara `orelse` chars

fmtpara = parafmtcmd `bind` \ cmd ->
	  unit (FmtPara cmd) `ap` richtext `chk` offparafmtcmd cmd 

chars = lit (\t->case t of
		  Chars cs -> Just (PlainChars cs)
		  _        -> Nothing)

charfmtcmd = lit (\t->case t of
			FmtCmd "nl"    -> Just NewLine
		        FmtCmd "np"    -> Just NewPage
			FmtCmd ('/':s) -> charcmd s >>= fmtChar Off
			FmtCmd s       -> charcmd s >>= fmtChar On
			_              -> Nothing)

fmtChar sw cmd = Just (FmtChar sw cmd)

parafmtcmd = lit (\t->case t of
			FmtCmd s -> paracmd s
			_	 -> Nothing)

offparafmtcmd cmd =
  lit (\t->case t of
	     FmtCmd ('/':s) | paracmd s==Just cmd -> Just cmd
	     _	                                  -> Nothing)


charcmd = assoc Just Nothing charCmds . map toLower

charCmds = [("bold",Bold),("italic",Italic),("fixed",Fixed),
	    ("smaller",Smaller),("bigger",Bigger),("underline",Underline),
	    ("subscript",Subscript)] -- and so on...

paracmd = assoc Just Nothing paraCmds . map toLower

paraCmds = [("center",Center),
	    ("flushleft",FlushLeft),("flushright",FlushRight),
	    ("indent",Indent),("indentright",IndentRight),
	    ("outdent",Outdent),("outdentright",OutdentRight),
	    ("excerpt",Excerpt),("paragraph",Paragraph),
            ("comment",Comment')] -- ...
