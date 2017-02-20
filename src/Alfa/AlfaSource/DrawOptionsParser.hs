module DrawOptionsParser(parseDrawOptions) where
import DrawOptions
import Fixity
import AlfParserMonad
import qualified AlfLex
import Char(isDigit,toLower)
import Debug2(trace)

infixl 2 #,<#

parseDrawOptions imported = parse (drawOptionsP imported) . AlfLex.lexanal

drawOptionsP imported =
  comp # unfoldGoalsP <# compactOnP <# hideTrivialLetP <# layoutStyleP
       <# declDetailP <# proofStyleP <# globalArgOptionsP
  where
    comp u c h l dd p ao d =
      mergeHiding imported ao
        d{unfoldGoals=u,compactOn=c,hideTrivialLet=h,
	  declDetail=dd,proofStyle=p,layoutStyle=l}

     -- should get rid of the hardwired defaults here...
    unfoldGoalsP = optionalP False ("unfoldgoals" `kpre` onP)
    compactOnP = optionalP True ("brief" `kpre` onP)
    hideTrivialLetP = optionalP False ("hidetypeannots" `kpre` onP)
    layoutStyleP = optionalP Wide (kw Wide "wide" `orP` kw Tall "tall")

    declDetailP = optionalP CompleteDecls
                     (kw JustNames "justnames" `orP`
                      kw NamesAndTypes "typesignatures")

    proofStyleP = optionalP NDProof (kw UglyProof "plain" `orP` kw NDProof "nd"
		  		     `orP` kw BrorProof "topdown")
 
--parseGlobalArgOptions = parse globalArgOptionsP . AlfLex.lexanal

( # ) = mapP
(<#) = apP

globalArgOptionsP =
  GlobalArgOptions # hidingP <# idsOptionsP

hidingP = optionalP True ("hiding" `kpre` onP)

onP = kw True "on" `orP` kw False "off"

idsOptionsP = manyP idOptionsP

idOptionsP = (,) # varP <# argOptionsP

varP = kw Var "var" <# idP
       `orP`
       kw Con "con" <# idP

idP = unescape # (idenP `orP` conP)
  where
    unescape "" = ""
    unescape ('\\':c:s) = c:unescape s -- hmm
    unescape (c:s) = c: unescape s

argOptionsP =
  ArgOptions # hideCntP <# fixityP <# displayAsP <# bitmapSourceP

hideCntP = optionalP 0 $ "hide" `kpre` numP

fixityP = optionalP Nonfix fixP
  where
    fixP =
      kw Infix "infix" <# assocP <# precP
      `orP`
      kw Distfix3 "distfix3" <# assocP <# precP
      `orP`
      kw Distfix3b "distfix3b" <# assocP <# precP
      `orP`
      kw Distfix4 "distfix4" <# assocP <# precP
      `orP`
      kw Distfix "mixfix" <# assocP <# precP
      `orP`
      kw Postfix "postfix" <# precP
      `orP`
      kw Quantifier "quantifier" `chkP` t"domain" <# onP
      `orP`
      rkw Big
      `orP`
      rkw Tuple
      `orP`
      rkw Fraction
      `orP`
      rkw ProofGoal
      -- ...

    rkw k = kw k (map toLower (show k))

assocP = optionalP NonAssoc $
	 kw LeftAssoc "leftassoc"
	  `orP`
	 kw RightAssoc "rightassoc"
	  `orP`
	 kw Assoc "assoc"

precP = optionalP 0 numP

displayAsP = maybeP $ const id # t"as" <# idP

bitmapSourceP = optionalP UseNormalFont
		("with" `kpre` (kw UseSymbolFont "symbolfont"
				`orP`
				kw  UseImageFile "imagefile"))

kpre s p = const  id # t s <# p

kw c s = unitP c `chkP` t s

t = checkP . AlfLex.Iden
