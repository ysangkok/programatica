module Dialogue (dialogueSession) where

import Grammar (MetaSymb (..), Term (..), Trm)
import IOGrammar (GFState, cfOfLanguage, grammarOfLanguage)
import SymbolTable () -- hbc workaround
import Tokens ()      --  - " -
import TC ()          --  - " -
import Macros (symid, zIdent, catOfMeta)
import Operations
import ExportGF
import Option --- hiding (Opt)  -- not supported by hbc
import Refine 
import Editing
import Char (toUpper, isSpace)
import System (system)

-- exported
dialogueSession env sp lang cat resume = do
  let g   = grammarOfLanguage lang env
      cf  = cfOfLanguage lang env
      gr  = (g,cf)
      sp0 = sp ---
  gfDialogue sp0 sp gr cat resume

-- type Grammar  = ((Abstract,Concrete),(CF,Cat)) -- in ExportGF
type Speech = String

type DState  = EState -- history of ambiguous states
emptyDState = emptyEState
initDState gr cat = eNew (abstract gr) (zIdent cat) emptyEState

gfDialogue :: Speech -> Speech -> Grammar -> Cat -> IO () -> IO ()
gfDialogue sp0 sp gr cat resume = diaLoop (initDState gr cat) 

 where

  diaLoop st = do
    mkMove st
    a   <- getAnswer st
    processAnswer st a

  mkMove st = case eStatus st of
    EEmpty -> mkWelcome
    EComplete -> mkConclusion (sti st)
    EWork -> mkNextQuestion (sti st)
    EAmbiguous -> mkAmbiguous
    EImpossible -> mkImpossible

  sti = errVal undefined . termInfoOfState
  abstr = abstract gr

  getAnswer st = do
    putStr "> "
    s <- getLine
    speakAloud sp0 s
    return s

  processAnswer st a = case parseAnswer gr st a of
    CRefine mts ->
      diaLoop $ eRefines abstr mts st
    CConstraint c ->
      diaLoop $ eAddConstrs abstr [mkConstraint c] st
    CUndo ->
      diaLoop $ eUndo st
    CMenu -> do
      mkMenuList st
      diaLoop st
    CStateNow -> do
      mkStateNow st
      diaLoop st
    CNoise -> do 
      mkFailUnderstand
      diaLoop st
    CAmbiguous -> do 
      mkMenuList st
      diaLoop st
    CQuit  -> do 
      mkBye
      resume

  mkWelcome         = linsp sp gr (mkConstant "Welcome")
  mkConclusion ti   = linsp sp gr (compute gr (termOfTI ti))
  mkStateNow st     = linsp sp gr $ compute gr $ errVal (mkConstant "Ambiguous") $
                                                     termOfState st
  mkNextQuestion ti = linsp sp gr (mkQuestion ti)
  mkFailUnderstand  = linsp sp gr (mkConstant "FailUnderstand")
  mkAmbiguous       = linsp sp gr (mkConstant "Ambiguous")
  mkImpossible      = linsp sp gr (mkConstant "Impossible")
  mkMenuList st     = saysp sp    (unlines (map (linearize gr) (mkMenu gr st)))
  mkBye             = linsp sp gr (mkConstant "Bye")


data Command =
   CRefine [(MetaSymb,Trm)]
 | CConstraint Trm
 | CMenu
 | CNoise
 | CAmbiguous
 | CUndo
 | CStateNow
 | CQuit

parseAnswer :: Grammar -> DState -> String -> Command
parseAnswer gr st s = 
  case prs of
        [[(_,Cons c)]] | symid c == "ShowMenu" -> CMenu
        [[(_,Cons c)]] | symid c == "Bye"      -> CQuit
        [[(_,Cons c)]] | symid c == "Undo"     -> CUndo
        [[(_,Cons c)]] | symid c == "StateNow" -> CStateNow
        [[(m,c)]] | symid (catOfMeta m) == "Constraint" -> CConstraint c
        [mts]  -> CRefine mts
        []     -> CNoise
        _      -> CAmbiguous

        --- add ambiguity 
        -- multiple refinements must be separated by ";"
      where 
        mcat  = ("MetaMove",  MetaSymb (zIdent "MetaMove",0))
        ccat  = ("Constraint",MetaSymb (zIdent "Constraint",0))
        cms   = mcat : ccat : [(symid $ catOfMeta m, m) | m <- metasOfState st]
        cms0  = take 2 cms     -- priority to first meta
        prs   = combinations $ map parse $ tkParts [] s 
        parse x = case parseInCats gr cms0 x of
           [] -> parseInCats gr cms x
           p  -> p
        tkParts ts t = case t of 
           [] -> ts
           _  -> tkParts (ts ++ [t1]) (dropWhile (==';') t2) 
                                   where (t1,t2) = span (/=';') t

parseInCats gr cms s = 
  [(m,t) | (c,m) <- cms, t <- parseCustom [ignoreParse] c gr s]

mkConstant :: String -> Trm
mkConstant = Cons . zIdent

mkConstraint c = (c,mkConstant "True")

mkQuestion :: TermInfo -> Trm
mkQuestion ti = case firstMetaOfTI ti of
  Ok (MetaSymb (cat,_)) -> mkConstant ("Quest" ++ symid cat)
  _ -> mkConstant "Sorry"

mkMenu :: Grammar -> DState -> [Trm]
mkMenu gr st = errVal  [mkConstant "Sorry"] $ do
   ti <- termInfoOfState st
   m  <- firstMetaOfTI ti
   return $ map fst $ refinementsOfGoal (abstract gr) ti m

-- for the speech synthesizer

linsp :: Speech -> Grammar -> Trm -> IO ()
linsp sp gr = saysp sp . linearize gr

saysp sp s = do
  putStrLn s
  speakAloud sp s

speakAloud :: Speech -> String -> IO ()
speakAloud "nospeech" s = putStrLn (">>>>" +++ map toUpper s +++ "<<<<")
speakAloud sp s = do
   system ("echo \"" ++ s ++ "\" | festival --tts" ++ sp') 
   return () 
 where
   sp' = if null sp then "" else " --language" +++ sp
