{-|
  Defines commands for emacsagda.

-}
module Commands( autoCommand, abstractCommand, addDefsCommand, caseCommand,
  commandError, computeCommand, computeSCommand, contUnfold1Command,
  contUnfoldNCommand, giveCommand, importDefsCommand, introCommand,
  introsCommand, letCommand, new, nfCountCommand, printConstraints,
  printContext, printTypeAllMeta, printTypeExp, printTypeMeta,
  refineCommand, refineExCommand, refinePrjCommand, solveAll1,
  solveCNumber, suggestCommand, termCountCommand, termDefsCommand,
  unfold1Command, unfoldCNumber ) where
import Maybe(maybeToList,catMaybes)
import AgdaTrace
import PPrint(ppReadable,PDetail(..),PPrint,pppr,ppDebug,pIText)
import FString(StrTable)
import Error(ErrMsg(..),EMsg,eMsg) 
import Lex (lexStart,lx)
import Position(Position(..),noPosition)
import Id(initST,UId,getUIdPosition,Id)
import CSyntax
import CPrinter
import CParser(CParser,pExpr,finalP,pLetDefs,pOper,pBindIds)
import MetaVarState(Constraint)
import MetaVars(MetaVar,preMetaVar)
import RemMetaVars(remMetaVars,remMetaVarsLetDef)
import Monads
import ProofState(initState)
import ProofMonad(PCM,getMetaInfo,putStrTable,getStrTable,getUninstantiated,isVisible)
import BasicOps
import List(nubBy)

commandError :: String -> PCM String
commandError s = raise (noPosition,ECommand s)



autoCommand ::  Position -> MetaVar ->  PCM ([(MetaVar,String,[MetaVar])])
autoCommand pos m = do e <- auto m
                       giveme m e
                       return []
                      
giveCommand :: Position -> MetaVar ->  String -> PCM ([MetaVar],Bool,[(MetaVar,String,[MetaVar])] )
giveCommand pos m s = do e        <- parseMeta m pos s
                         e' <- giveme m e
                         let (_,ms)  = remMetaVars e'
                         mes <- latestMetaSubsts
                         mes' <- mapM metaStrRemMetaVars mes
                         needsPar <-((precCExpr e)<=)`fmap`getMetaInfo_pi m
                         return (ms,needsPar,catMaybes mes')

addDefsCommand :: Position -> String -> PCM ([MetaVar],[(MetaVar,String,[MetaVar])] )
addDefsCommand pos s = do 
        ds <- pParse pLetDefs pos s
        --traceM ("parsed succeeded "++ ppReadable ds)
        (ds',ds3) <- addDefs ds
        --traceM ("TC succeeded "++ ppReadable ds3)
        --traceM (ppReadable ds')
        --traceM (ppReadable ds3)
        let ds2 = map remMetaVarsLetDef ds'
        --traceM ("remDefs " ++ ppReadable ds2)
        mes <- latestMetaSubsts
        mes' <- mapM metaStrRemMetaVars mes
        --traceM ("Remaining ctrs " ++ ppDebug mes')
        return (concatMap snd ds2,catMaybes mes')


importDefsCommand :: Position -> String -> PCM ()
importDefsCommand= wrapLetDefs$ (>> done).importDefs

termDefsCommand :: Position -> String -> PCM (Maybe EMsg)
termDefsCommand = wrapLetDefs$ fmap chkerr . checkCorrDefs where
 chkerr(xs,yys) = case map termE xs ++ map positE yys of
                   []    -> Nothing
                   [err] -> Just err
                   errs  -> Just . eMsg noPosition $ EMany errs
 termE x = eMsg (getUIdPosition x) (WNotTerminate (ppReadable x))
 positE(ys,y) = eMsg (getUIdPosition y) (WNotPositive (ppRM ys) (ppReadable y))

refineCommand, refineExCommand -- Ex now use parseMeta instead of pParse pExpr
   :: Position -> MetaVar ->  String -> PCM [(MetaVar,String,[MetaVar])]
(refineCommand, refineExCommand) = (go refine, go refineExact) where
  go f pos m s = (parseMeta m pos s>>=toStrMVs f m)`handle` tryOp where
    tryOp(_,ESyntax _ _)= do op <- pParse pOper pos s 
                             let e = toexp op
                             e' <- refine m e
                             mes <- latestMetaSubsts
			     toStrMVE m e' mes
                
    tryOp err = raise err
    toexp op  = CBinOp m0 op m0 where m0=CMeta noPosition False (Just False) preMetaVar

caseCommand :: Position -> MetaVar -> String -> PCM ([(MetaVar,String,[MetaVar])])
caseCommand = wrapMetaExp$ toStrMVs $ makeCase

abstractCommand :: Position -> MetaVar -> String -> PCM [(MetaVar,String,[MetaVar])] 
abstractCommand pos m s = do 
    putPosition m pos 
    xs          <- pParse pBindIds pos s
    (e,me') <-  abstract m xs
    mes <- latestMetaSubsts
    msms        <- mapM metaStrRemMetaVars mes 
    (e',ms)     <- strRemMetaVars m e
    let msms'    = (m,e',ms):(catMaybes msms)
    case me' of 
        Nothing      -> return msms'
        Just (mt,et) -> strRemMetaVars mt et >>= \(s,ms) -> return$ (mt,s,ms):msms'
                             
letCommand :: Position -> MetaVar -> String -> PCM ([(MetaVar,String,[MetaVar])])
letCommand = wrapMetaIds $ toStrMVs $ let_def

refinePrjCommand :: Position -> MetaVar -> String -> PCM ([(MetaVar,String,[MetaVar])])
refinePrjCommand pos m s = do 
    putPosition m pos 
    is       <- pParse pBindIds pos s 
    e' <- case is of 
            (i:is) -> refinePrj m i is 
            _      -> raise $ eMsg noPosition $ ECommand "refinePrj with < 2 ids"
    mes <- latestMetaSubsts
    toStrMVE  m e' mes 

introsCommand :: Position -> MetaVar -> PCM String
introsCommand _ = fmap (unlines . map ppReadable) . intros

suggestCommand :: PCM [String]
suggestCommand = suggestSolution >>= mapM pr where
  pr(m,e)=(\ (s,_)-> '?':show m++" =\n"++s++"\n")`fmap`strRemMetaVars m e

solveAll1    ::               PCM [(MetaVar,String,[MetaVar])] 
solveCNumber :: Int -> Int -> PCM [(MetaVar,String,[MetaVar])]
(solveAll1,solveCNumber)=(go (solveAllCsSafeRep 500), (go.).solveCN) where
  go com = do
        com 
        mes <- latestMetaSubsts
        mes' <-  mapM metaStrRemMetaVars mes
        return$ catMaybes mes'
unfoldCNumber = unfoldCN

introCommand :: Position -> MetaVar -> PCM ([(MetaVar,String,[MetaVar])])
introCommand _ m = do 
     e <- intro m 
     mes <- latestMetaSubsts
     toStrMVE m e mes

{-
listMetas :: MetaVar -> PCM [MetaVar]
listMetas m = do e <- getMetaSubst m; return (listMetaVars e)
-}

new :: PCM ()
new = updateSTM (\_ -> initState)

pParse :: CParser a -> Position -> String -> PCM a
pParse p (Position s l c) inp = lx False s l c `flip` inp `fmap` getStrTable
  >>= liftESTM . finalP p >>= \ (a,st)-> putStrTable st >> return a 

parseMeta :: MetaVar -> Position -> String -> PCM CExpr
parseMeta m pos s = do putPosition m pos
                       --(_,pai,_,_,_,_,_,_)<- getMetaInfo m
                       --pParse (if pai then pExpr else pExpr) pos s
                       pParse pExpr pos s
-- | PJ: Why use an if with identical branches? A disguised seq?
-- | CC : There used to be a difference but maybe this check is done during translation

strRemMetaVars :: MetaVar -> CExpr -> PCM (String,[MetaVar])
strRemMetaVars m e = do
  let (e',ms) = remMetaVars e 
  s<-pppr PDReadable `flip` e' `fmap` getMetaInfo_pi m
  return (s,ms)

metaStrRemMetaVars (aut,m,e) = do 
        vis <- getVisibilityMeta m
        if not (isAutomatic vis)
           then do (s,ms) <- strRemMetaVars m e 
                   return$ Just (m,s,ms)
           else return Nothing 

{-
        if not (isAutomatic vis)
           then if aut 
                   then return $ Just (m,"_",[])
                   else do (s,ms) <- strRemMetaVars m e 
                           return$ Just (m,s,ms)
           else return Nothing 
-}
--metaStrRemMetaVars' (aut,m,e) = do (s,ms)<- strRemMetaVars m e; return(m,s,ms)


printTypeMeta :: Bool -> Int -> MetaVar -> PCM String -- i==0 => skip unfold
printTypeMeta showPos i m  = do
        stype <- ((fmap (pIText.pPrintCJudg PDReadable).).getTypeOfMeta) i m
        pos<- getPosMeta m
        return ((if showPos then "Close to: " ++ show pos  else "")++ "\n"++stype)
 
printTypeAllMeta :: PCM String
printTypeAllMeta = unlines`fmap`(mapM(printTypeMeta True 0)=<<getUninstantiated)

printTypeExp :: Position -> Int -> MetaVar -> String -> PCM String
printTypeExp pos i m s = do --traceM (show s)
                            ppwrapXExp (getTypeExp i . Just) pos m s

printConstraints :: PCM String
printConstraints = pr `fmap` constraints where
  pr        = concatMap pr1 . nubBy dup . zip [0..] . map ppDebug
  pr1 (i,s) = show i++":\n"++s++"\n"
  dup (_,s1)(_,s2) = s1 == s2

printContext :: MetaVar -> PCM String 
printContext = fmap (unlines . map ppReadable) . getContext

computeCommand, computeSCommand :: Position -> (Maybe MetaVar) -> String -> PCM String
computeCommand  = ppwrapXExp computeWHNF
computeSCommand = ppwrapXExp computeWHNFS
 
unfold1Command   :: Position -> MetaVar -> String -> PCM String
unfold1Command  = ppwrapXExp (unfoldN 1 . Just)

contUnfold1Command :: PCM String
contUnfold1Command = ppRM$ contUnfold1

nfCountCommand :: Position -> MetaVar -> Int -> String -> PCM String
nfCountCommand pos m n = ppwrapExp (nfC n m) pos

contUnfoldNCommand :: Int -> PCM String
contUnfoldNCommand n = ppRM$ if n==1 then contUnfold1 else contUnfoldN n

termCountCommand :: Int -> PCM ()
termCountCommand = setTerminationCounter 

ppRM::(PPrint a,Functor m)=> m a -> m String
ppRM = fmap ppReadable

type PS_PCM a    = Position ->      String -> PCM a
type PXS_PCM x a = Position -> x -> String -> PCM a
wrapX   :: CParser x -> (x -> PCM b) -> PS_PCM b
ppwrapX :: (PPrint b)=> CParser x -> (x -> PCM b) -> PS_PCM String
ppwrapExp  :: (PPrint b)=> (     CExpr -> PCM b) ->  PS_PCM   String
ppwrapXExp :: (PPrint b)=> (x -> CExpr -> PCM b) -> PXS_PCM x String
wrapX     pX op pos s   = pParse pX pos s >>= op
ppwrapX   pX op         = wrapX pX (ppRM . op)
ppwrapExp               = ppwrapX pExpr
ppwrapXExp op pos x     = ppwrapExp (op x) pos
wrapLetDefs             = wrapX pLetDefs
wrapMetaX pX op pos m s = putPosition m pos >> pParse pX pos s >>= op m
wrapMetaExp             = wrapMetaX pExpr
wrapMetaIds             = wrapMetaX pBindIds


toStrMVs op m e = do e' <- op m e 
                     mes <- latestMetaSubsts
                     toStrMVE m e' mes

toStrMVE :: MetaVar -> CExpr -> [CMetaSubst]
            -> PCM [(MetaVar,String,[MetaVar])]
toStrMVE m e mes = do mes'   <- mapM metaStrRemMetaVars mes
                      (s,ms) <- strRemMetaVars m e
                      return ((m,s,ms):catMaybes mes')
