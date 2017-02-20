import ListUtil(chopList,assoc)
import List(nub,partition,groupBy,sortBy,(\\))
import Char(isSpace,isUpper,toLower)
import IOUtil(getEnvi)
import Maybe(fromMaybe)
import Unsafe(unsafeOpenFile)
import Syntax
import ParseFile
import PP
import SyntaxUtils
import HtmlUtils
import Utils(mix,mapPartition,pairwith)
import ListUtil(mapFst)
import HO(apFst)
import Time(getClockTime,toCalendarTime,calendarTimeToString)

import qualified OrdMap as Map
type Map a b = Map.OrdMap a b

#ifdef __HASKELL98__
#define map fmap
#endif

--fudgetDir = "/usr/src/local/lml/lmlx/"
fudgetDir = "../../"
fudgetDocDir = fudgetDir++"Doc/latexinfo/"
--fudgetLibDir = fudgetDir++"lib/"
fudgetsHi = "sections"
smallfudgetsHi = fudgetDir++"hsrc/Fudgets.hi"
fudgetManDir = "FudgetMan/" -- directory for HTML output
fudgetManIndex = fudgetManDir++fullIndexFile
fudgetManBeginnerIndex = fudgetManDir++"beginner.html"
fudgetManSmallIndex = fudgetManDir++"small.html"
fudgetManOldIndex = fudgetManDir++"old.html"

libraryName = fromMaybe "Fudget Library" $ getEnvi "LIBNAME"
fullIndexFile = fromMaybe "index.html" $ getEnvi "FULLINDEXFILE"

main =
  do today <- getTodaysDate
     docentries <- (doc . lines) `map` getContents
     --putStr (show docentries) -- debugging
     writeIt today docentries
  where
    getTodaysDate =
       calendarTimeToString `map` (getClockTime >>= toCalendarTime)

writeIt today docentries0 =
  do let names = map fst docentries
         crossrefs = refpage (map classfree names)
	 docentries = mapFst (escsynopsis crossrefs) docentries0
     writeIndices today ("",crossrefs) names
     let (images,docentries'') = unzip $ map fexamples docentries
     writeFile "fexample.hs2" (fexample2hs (concat images)) 
     writeEntries crossrefs docentries''
  where
    escsynopsis es (this,others,cs,ws) = (this,others,cs,synopsis)
      where synopsis = escapechars ("",es) (unwords ws)

writeEntries crossrefs = mapM_ (writeEntry crossrefs)

writeEntry crossrefs (n@(this,_,_,_),ls) =
  do let file = fudgetManDir++this++".html"
     writeFile' file (unlines $ docentry crossrefs n ls)

doc (('$':'D':'o':'c':' ':cs):ls)=
       let this:others = words cs
	   (ce,ls')    = enddoc [] ls
	   (c,e0)      = docclass ce
	   (sy,e)      = docsynopsis e0
       in ((this,others,c,sy),e):doc ls'
doc (_:ls) = doc ls
doc [] = []

enddoc es (('$':'E':'n':'d':'d':'o':'c':_):ls) = (reverse es,ls)
enddoc es (('\\':'c':[]   ):ls) = enddoc es ls
enddoc es (('\\':'c':t:_):ls) | isSpace t = enddoc es ls
enddoc es (l:ls) = enddoc (l:es) ls
enddoc es [] = (reverse ("$Enddoc":es),[])

docclass = extractlines "$C"
docsynopsis = extractlines "$Synopsis"

extractlines w1 = apFst concat . mapPartition match
  where match l =
           case words l of
	     w:ws | w==w1 -> Just ws
	     _ -> Nothing

classfree (this,others,c,syn) = (this,others)

docentry es (this,others,_,synopsis) ls =
    on "HTML":
    docfmt crossrefs (this:others) synopsis ++
    usage this argnames ++
    concatMap (para args crossrefs) (arghdr ls) ++
    [on "!--#include file=\"footer.html\"--",off "BODY",off "HTML"]
  where
    crossrefs = (this,es)
    args = zip argnames argtypes
    argnames = [a|"$Arg":a:_<-map words ls]
    argtypes = case Map.lookup this values of
                 Just (Sig _ (CT _ t)) -> funArgs t
		 _ -> []

para args es "" = ["<P>"]
para args es ('$':cs) | kind/="this" =
      parafmt args es kind (escapechars es txt)
    where (kind,txt) = takeWord cs
para args es cs = [escapechars es cs]

parafmt args es kind txt =
    assoc (\f->f (args,es) txt) (defaultpara kind txt) parafmts kind
  where
    defaultpara kind txt = [wrap "H2" kind ++ txt]

    parafmts = [("Input",const $ iofmt "Input"),
		("Output",const $ iofmt "Output"),
		("Type",\(_,es)->typefmt es.fst.firstWord),
		("Arguments",const arghdrfmt),
		("Argend",const argendfmt),
		("Arg", argfmt),
		("Example", const examplefmt),
		("MExample",const examplefmt),
		("FExample",const examplefmt),
		("See",const seefmt)
	       ]

docfmt es ns synopsis =
    title2 (libraryName ++ " Manual: "++tit) ("¤ "++tit) $ 
    synopsis :
    wrap "H2" "Types" :
    on "PRE":concatMap (idDef es) ns++[off "PRE"]
  where
    tit = mix (take maxnames ns++etal) ", "
    etal = if null (drop maxnames ns) then [] else ["et al"]
    maxnames = 2

idDef es name@(c:_) =
    if isUpper c
    then ppTypeDef es (typeDef name) ++ concatMap (ppTopDecl es) instances
    else ppValueDef es (valueDef name) ++infixes
  where
    instances = nub [d | Interface _ ds<-fudgetsHiLines,
                         d@(Instance _ (Con cl (ConT (Con n _):_)))<-ds,
			 n==name || cl==name,cl/="Eval"]
    infixes = case lookup name infixdecs of
                  Just d -> ppTopDecl es d
		  _ -> []

title txt = title2 txt txt

title2 title header rest =
    on "HEAD" :
    --stylelink :
    wrap "TITLE" (escape title) :
    off "HEAD" :
    on "BODY" :
    on "!--#include file=\"header.html\"--" :
    wrap "H1" (escape header) : rest
  where
    stylelink =
      tag "LINK REL=\"STYLESHEET\" TYPE=\"text/css\" HREF=\"manual.css\""

usage _ [] = []
usage this args =
  [wrap "H2" "Synopsis",wrap "PRE" (unwords (this:args))]

arghdr [] = []
arghdr (l:ls) =
  case words l of
    "$Arg":_ -> "$Arguments":l:argend ls
    _ -> l:arghdr ls

argend [] = ["$Argend"]
argend lls@(l:ls) =
  case words l of
    ('$':w):_ | w `notElem` ["this","Arg"] ->
                  if w=="Argend"
		  then lls
		  else "$Argend":lls
    _ -> l:argend ls

typefmt es t = [wrap "PRE" (unlines (ppTypeDef es (typeDef t)))]
iofmt h txt= [wrap "H3" h++txt]

argfmt (args,crossrefs) txt =
  let (arg,ws) = firstWord txt
      typ = assoc (ppType crossrefs) "??" args arg
  in [(wrap "DT".wrap "CODE") (wrap "B" arg ++ " :: "++typ) ++ on "DD" ++ ws]

arghdrfmt _ = [wrap "H2" "Arguments",on "DL"]
argendfmt _ = [off "DL"]

examplefmt txt = [wrap "H2" "Example",wrap "PRE" txt]
seefmt txt = [wrap "H2" "See Also"++txt]
--enddocfmt _ = ["<HR>"]

escapechars es@(this,_) = replacethis (wrap "CODE" this) . chars es

replacethis this cs =
  case cs of
    "" -> ""
    '$':'t':'h':'i':'s':rest -> this++replacethis this rest
    c:rest -> c:replacethis this rest

chars es "" = ""
chars es cs =
  case break (=='\\') cs of
    (_,"") -> escape cs
    (before,'\\':rest) ->
      case break (\c->c=='{' || isSpace c) rest of
        (_,"") -> escape cs
	(cmd,rest@(c:_)) | isSpace c ->
	   escape before++charcmd cmd ++ chars es rest
	(cmd,'{':rest) ->
	  case break (=='}') rest of
	    (_,"") -> escape cs
	    (arg,'}':rest) -> escape before++charfmt es cmd arg++chars es rest

charcmd cmd = assoc on ('\\':cmd) charcmds cmd

charcmds = [("item","LI")]

charfmt es cmd arg = assoc (\f->f es arg) (defaultchars cmd arg) charfmts cmd

charfmts = [("emph",const $ wrap "EM".escape),
            ("code",const $ wrap "CODE".escape),
            ("strong",const $ strong.escape),
            ("key",const $ wrap "KBD".escape),
            ("kbd",const $ wrap "TT".escape),
	    ("xref",ppName),
	    ("link",const extref),
	    ("input",inputchars),
	    ("image",const imgsrc),
	    ("begin",const beginchars),
	    ("end",const endchars)
	   ]

refpoint arg =
  let a = escape arg
  in on ("A NAME="++dq a)++a++off "A"

intref arg =
  let a = escape arg
  in on ("A HREF="++dq ("#"++a))++a++off "A"

extref arg =
  let (url,link) = takeWord arg
      link' = if all isSpace link then url else link
  in on ("A HREF="++dq url)++link'++ off "A"

inputchars es arg =
  case unsafeOpenFile (fudgetDocDir++arg++".tex") of
    Left _ -> "\\input{"++arg++"}"
    Right s -> escapechars es s -- "$this" is not present in s...

beginchars = assoc on "" charfmtcmds
endchars = assoc off "" charfmtcmds

charfmtcmds = [("example","PRE"),
               ("itemize","UL"),
               ("enumerate","OL")
	      ]

defaultchars cmd arg = '\\':cmd++"{"++escape arg++"}"

takeWord = break isSpace
firstWord = takeWord . dropWhile isSpace

smallNames = [defName d | Interface _ ds <- optReadHiFile smallfudgetsHi, d<-ds]

fudgetsHiLines = readHiFile fudgetsHi

optReadHiFile = readHiFile' (const [])
readHiFile = readHiFile' error

readHiFile' error f = 
   case unsafeOpenFile f of
     Left s -> error (f++": "++s)
     Right s -> case parseFile s of
                  Right i -> i
		  --Left err -> error (f++": syntax error: "++show err)
		  Left (expected,got) ->
                     error (f++": syntax error, expected one of: "++
			    unwords expected++", but got "++take 100 (unwords got))

types = [(defName d,d) | Interface _ ds <- fudgetsHiLines, d <- ds, isType d]

values =
    Map.fromList
      [(name,s) | Interface _ ds <- fudgetsHiLines,
                  Value s@(Sig name _) <- ds]

infixdecs = [(name,d) | Interface _ ds <- fudgetsHiLines,
                         d@(Infix _ _ name)<-ds]

--typeDef t = assoc id ("type "++t++" = ??") types t

typeDef t =
    (t,
    case [d | (n,d)<-types,n==t] of
      [] -> Nothing
      d@(Data _ cs):ds ->
        case cs of
	  Just _ -> Just d
	  Nothing ->
	    case [d | d@(Data _ (Just _)) <- ds] of
	      [] -> Just d
	      d:_ -> Just d
      d:_ -> Just d)

valueDef v = (v,Map.lookup v values)

writeIndices today crossrefs entries =
  do --appendChan stdout (show beginnerHiLines) -- debugging
     writeFile' fudgetManBeginnerIndex beginnerIndex
     writeFile' fudgetManSmallIndex smallIndex
     writeFile' fudgetManIndex fullIndex
     writeFile' fudgetManOldIndex oldIndex
  where
    fullIndex = ixFile fullHiLines "Full Index"
    beginnerIndex = ixFile beginnerHiLines "Beginner's Index"
    smallIndex = ixFile smallHiLines "Programmer's Index"
    oldIndex = ixFile oldHiLines "Index of obsolete stuff"
    ixFile hi ixn = unlines $ fudgetIndex today hi ixn crossrefs synopses
    beginnerNames = [name|(this,others,cs,_)<-entries,
                     "beginner" `elem` cs, name<-this:others]
    synopses = Map.fromList
                  [(name,synopsis)|(this,others,_,synopsis)<-entries,
		                   not (null synopsis),
		                   name<-this:others]
    fullHiLines = filterHiLines (const True)
    beginnerHiLines = filterHiLines (`elem` beginnerNames)
    smallHiLines = filterHiLines (`elem` smallNames')
    oldHiLines = filterHiLines (`elem` oldNames)
    smallNames' = smallNames \\ oldNames
    oldNames = [name | (this,others,cs,_)<-entries,
		       "old" `elem` cs,
		       name<-this:others]
    filterHiLines include =
      [Interface n ds | Interface n ds0 <- fudgetsHiLines,
                        let ds = [d | d<-ds0,not (isInstance d),
				      include (defName d)],
                        not (null ds)]

fudgetIndex today hiLines indexName crossrefs synopses =
    on "HTML":
    (title2 (manualName++" - "++indexName)
            ("¤ "++manualName++" ¤") .
     (("Created from the "++libraryName++" sources on "++today):) .
     (wrap "H2" indexName:).
     indexSections) hiLines ++
     [off "BODY",off "HTML"]
  where
    manualName = libraryName ++ " Reference Manual"

    indexSections ss =
      sectionList [n | Interface n _<-ss] ++
      concatMap (fmtsection.ordersection) ss

    ordersection (Interface n s) =
        Interface n (nub types ++ rmdups (sort values))
      where (types,values) = partition isType s
            sort = sortBy by
            by d1 d2 = defName d1 `compare` defName d2

	    rmdups (x1:xs@(x2:_)) = if x1==x2
	                            then rmdups xs
				    else x1:rmdups xs
	    rmdups xs = xs

    fmtsection (Interface s ls) =
        wrap "H2" (refpoint s):
	on "DL COMPACT":
       (concatMap fmtGrp . grp . map (pairwith syn) $ ls) ++
	[off "DL",on "HR"]
      where
        syn def = Map.lookup (defName def) synopses
	grp = moveMisc . map extr . groupBy eqSnd . sortBy cmpSnd
	  where
	    moveMisc (d@(Nothing,_):ds) = ds++[d]
	    moveMisc ds = ds
	    extr ((def,syn):ds) = (syn,def:map fst ds)
	    eqSnd = opSnd (==)
	    cmpSnd = opSnd compare
	    opSnd op (x1,y1) (x2,y2) = op y1 y2
        fmtGrp (syn,defs) =
	    fmtSyn (fromMaybe "Miscellaneous (the rest)" syn):
	    concatMap fmtDef defs
	  where
	    fmtSyn s = on "DT"++s++":"
	    fmtDef d =
	      on "DD": on "CODE" : ppTopDecl crossrefs d ++ [off "CODE"]

{-
    fmtsection (Interface s ls) =
        wrap "H2" (refpoint s):
	--on "P":
	on "DL":
	concatMap fmtline ls ++
	[off "DL",on "P",on "HR"]
      where
        fmtline d =
	    on "DT": on "CODE": ppTopDecl crossrefs d ++ [off "CODE"]++ synopsis
	  where
	    synopsis =
	      case Map.lookup (defName d) synopses of
	        Just s -> [on "DD"++s]
		_ -> []
-}

    sectionList ss =
      wrap "H2" "Sections":
      on "UL":
      [on "LI"++intref s|s<-ss]++
      [off "UL",tag "HR"]

fexamples (n@(this,_,_,_),ls) = (is,(n,ls'))
  where
    (is,ls') = fexamples' 0 [] ls
    fexamples' _ is [] = (is,[])
    fexamples' n is (cs:ls) =
       let (is',ls') = fexamples' n' is ls
           (kind,txt)  = takeWord cs
	   (n',is'',ls'') = if kind=="$FExample"
	                    then let imgname=this++show n'
			         in (n+1,
			             (imgname,txt):is',
				     ("\\image{"++imgname++".gif"++"}"):ls')
			    else (n,is',ls')
       in (is'', cs:ls'')

fexample2hs examples = unlines (
  "examples = ":
  ["  (\""++name++"\", stubF "++f name++") :" | (name,_) <- examples] ++
  ["  []",""] ++
  [f name ++ " = " ++ ex | (name,ex) <- examples])
  where
    f (c:cs) = toLower c:cs -- Let's hope this doesn't create any name clashes!

refpage names = [(n,page) | (page,ns) <- names, n<-page:ns]

writeFile' name s = putStrLn name >> writeFile name s
