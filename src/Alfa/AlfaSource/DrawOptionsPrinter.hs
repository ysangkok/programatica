module DrawOptionsPrinter(printDrawOptions,printIdOptions) where
import DrawOptions
import Fixity
import PPrintTH

printDrawOptions = pprint . prDrawOptions
--printGlobalArgOptions = pprint . prGlobalArgOptions
printIdOptions x = pprint . prIdOptions $ x

prDrawOptions d =
    "unfoldgoals" & prOn (unfoldGoals d) !/
    "brief" & prOn (compactOn d) !/
    "hidetypeannots" & prOn (hideTrivialLet d) !/
    prLayoutDir (layoutStyle d) !/
    prDeclDetail (declDetail d) !/
    prProofStyle (proofStyle d) !/
    prGlobalArgOptions (argOpts d)
  where
    prLayoutDir Tall = "tall"
    prLayoutDir Wide = "wide"

    prDeclDetail JustNames = pr "justnames"
    prDeclDetail NamesAndTypes = pr "typesignatures"
    --prDeclDetail CompleteDecls = pr "complete"
    prDeclDetail _ = nil

    prProofStyle UglyProof = "plain"
    prProofStyle NDProof = "nd"
    prProofStyle BrorProof = "topdown"

prGlobalArgOptions (GlobalArgOptions hidingOn idsOptions) =
  "hiding" & prOn hidingOn ! nl !
  prIdsOptions idsOptions

prOn True = "on"
prOn False = "off"

prIdsOptions = vpr . map prIdOptions

prIdOptions (name,argOptions) =
  prName name & prArgOptions argOptions

prName (Var s) = "var" & showId s
prName (Con s) = "con" & showId s

prArgOptions (ArgOptions hideCnt idFixity displayAs bitmapSource) =
  prHideCnt hideCnt &
  prFixity idFixity &
  prDisplayAs displayAs &
  prBitmapSource bitmapSource

prHideCnt 0 = nil
prHideCnt n = "hide" & n

prFixity fixity =
  case fixity of
    Nonfix -> nil
    Infix assoc prec -> "infix" & prAssoc assoc & prPrec prec
    Distfix3 assoc prec -> "distfix3" & prAssoc assoc & prPrec prec
    Distfix3b assoc prec -> "distfix3b" & prAssoc assoc & prPrec prec
    Distfix4 assoc prec -> "distfix4" & prAssoc assoc & prPrec prec
    Distfix assoc prec -> "mixfix" & prAssoc assoc & prPrec prec
    Postfix prec -> pr "postfix" & prPrec prec
    Quantifier domain -> "quantifier domain" & prOn domain
    Big -> pr "big"
    Tuple -> pr "tuple"
    Fraction -> pr "fraction"
    ProofGoal -> pr "proofgoal"
    -- ...

prPrec 0 = nil
prPrec n = pr n

prAssoc a =
  case a of
    LeftAssoc -> pr "leftassoc"
    RightAssoc -> pr "rightassoc"
    Assoc -> pr "assoc"
    _ -> nil


prDisplayAs Nothing = nil
prDisplayAs (Just s) = "as" & showId s

prBitmapSource src =
  case src of
    UseNormalFont -> nil
    UseSymbolFont -> "with" & "symbolfont"
    UseImageFile -> "with" & "imagefile"

showId s = '"':concatMap escape s++"\""
  where
   escape '"' = "\\\""
   escape '\\' = "\\\\"
   escape c = [c]
