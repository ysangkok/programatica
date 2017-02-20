module RemMetaVars where

import CSyntax
import Util
import MetaVars
import AgdaTrace(trace)
import PPrint
import CPrinter

remMetaVarsProgram :: CProgram -> (CProgram,[MetaVar])
remMetaVarsProgram (CProgram ds) = let (ds',ms) = unzipWith remMetaVarsModule ds     
                                   in (CProgram ds',concat ms)


remMetaVarsModule :: CModule -> (CModule,[MetaVar])
remMetaVarsModule (CModule i as b) = 
                                let (as',ms) = unzipWith remMetaVarsCArg as
                                    (b',ms') = remMetaVarsPackageBody b
                                in (CModule i as' b',concat ms ++ ms')



remMetaVars :: CExpr -> (CExpr,[MetaVar])
remMetaVars (Clam b e) = let (b',ms) = remMetaVarsCArg b
                             (e',ms') = remMetaVars e
                         in (Clam b' e',ms++ms') 
remMetaVars (CUniv b e) = let (b',ms) = remMetaVarsCArg b
                              (e',ms') = remMetaVars e
                          in (CUniv b' e',ms++ms')
remMetaVars e@(CArrow b e0 e1) = let (e2,ms) = remMetaVars e0
                                     (e3,ms') = remMetaVars e1
                               in (CArrow b e2 e3,ms++ms')
remMetaVars (Clet ds e) = let (ds',ms1) = unzipWith remMetaVarsLetDef ds
                              (e',ms2) = remMetaVars e
                          in (Clet ds' e',concat ms1++ms2)
remMetaVars (CProduct pos cs) = let (cs',ms) = unzipWith remMetaVarsCSign cs
                                in (CProduct pos cs',concat ms)

remMetaVars (CRecord pos ps  ds) = 
               let (ds',ms) = unzipWith remMetaVarsLetDef ds
               in (CRecord pos ps ds',concat ms)

remMetaVars (Copen e1  (COpenArgs oas) e2) = 
                     let (oas',ms) = unzipWith remMetaVarsOpen oas
                         (e1',ms1) = remMetaVars e1
                         (e2',ms2) = remMetaVars e2
                     in (Copen e1' (COpenArgs oas) e2',concat ms ++ ms1++ms2)
remMetaVars (CSelect e i) = let (e',ms) = remMetaVars e
                            in (CSelect e' i,ms)
remMetaVars (CSum cs) = let (cs',ms) = unzipWith remMetaVarsCSummand cs
                        in (CSum cs',concat ms)
remMetaVars (CIndSum tel cs) = let
      (tel',m1) = unzipWith remMetaVarsCArg tel
      (cs',m2) = unzipWith remMetaVarsIndCb cs
   in (CIndSum tel' cs', concat(m1++m2))
remMetaVars (CCon i e) = let (e',ms) = remMetaVars e
                         in (CCon i e',ms)
remMetaVars (Ccase e arms) = let (e',ms) = remMetaVars e
                                 (arms',ms') = unzipWith remMetaVarsArm arms
                             in (Ccase e' arms',ms++concat ms')
remMetaVars (CApply e es) = let (e',ms) = remMetaVars e
                                (es',ms') = unzipWith remMetaVarsBoolArg es
                            in (CApply e' es',ms++(concat ms'))
remMetaVars (CBinOp e1 i e2) = let (e1',ms1) = remMetaVars e1
                                   (e2',ms2) = remMetaVars e2
                               in (CBinOp e1' i e2',ms1++ms2)
remMetaVars (CMeta pos pai aut m) = (CMeta pos pai aut preMetaVar, if (isAutomatic aut) then [] else [m])

remMetaVars (Cif e1 e2 e3) = let (e1',ms1) = remMetaVars e1
                                 (e2',ms2) = remMetaVars e2
                                 (e3',ms3) = remMetaVars e3
                             in (Cif e1' e2' e3',ms1++ms2++ms3)
remMetaVars e = (e,[])
           

remMetaVarsBoolArg :: (Bool,CExpr) -> ((Bool,CExpr),[MetaVar])
remMetaVarsBoolArg (b,e) = let (e',ms) = remMetaVars e
                           in  ((b,e'),ms)

remMetaVarsDef :: CDef -> (CDef,[MetaVar])
remMetaVarsDef (CDef ps d) = let (d',ms) = remMetaVarsCDefn d
                                in (CDef ps d',ms)
remMetaVarsDef d = (d,[])

remMetaVarsLetDef :: CLetDef -> (CLetDef,[MetaVar])
remMetaVarsLetDef (CSimple d) = let (d',ms) = remMetaVarsDef d
                                in (CSimple d',ms)
remMetaVarsLetDef (CMutual ds) = let (ds',ms) = unzipWith remMetaVarsDef ds
                                 in (CMutual ds',concat ms)
remMetaVarsLetDef d = (d,[])

remMetaVarsCDefn :: CDefn -> (CDefn,[MetaVar])
remMetaVarsCDefn d@(CValueT i as a e) = 
            let (as',ms1) = unzipWith remMetaVarsCArg as
                (a',ms2) = remMetaVars a
                (e',ms3) = remMetaVars e
            in (CValueT i as' a' e',concat ms1++ms2++ms3)
remMetaVarsCDefn d@(Cnewtype i as a cs) = 
            let (as',ms1) = unzipWith remMetaVarsCArg as
                (a',ms2) = remMetaVars a
                ([e'],ms3) = unzipWith remMetaVarsCSummand [cs]   --- Change!!
            in (Cnewtype i as' a' e',concat ms1++ms2++concat ms3)
remMetaVarsCDefn (Ctype i as a) =
             let (as',ms) = unzipWith remMetaVarsCArg as
                 (a',ms') = remMetaVars a
             in (Ctype i as' a',concat ms++ms')
remMetaVarsCDefn (Cdata i as mt cs) = 
        let (as',ms) = unzipWith remMetaVarsCArg as
            (mt',ms') = maybe (Nothing,[]) ((\p -> (Just (fst p), snd p)) . remMetaVars) mt
            (cs',ms2) = unzipWith remMetaVarsCSummand cs
        in (Cdata i as' mt' cs', concat ms++ms'++concat ms2)
remMetaVarsCDefn (CValue i e) = let (e',ms) = remMetaVars e
                                in (CValue i e' ,ms)

remMetaVarsCDefn (CAxiom i as a) = 
          let (as',ms) = unzipWith remMetaVarsCArg as
              (a',ms') = remMetaVars a
          in (CAxiom i as' a',concat ms++ms')

remMetaVarsCDefn (CNative i a) = 
          let (a',ms') = remMetaVars a
          in (CNative i  a',ms')
remMetaVarsCDefn (CPackage i as e) = 
          let (as',ms) = unzipWith remMetaVarsCArg as
              (e',ms') = remMetaVarsPackageBody e
          in (CPackage i as' e',concat ms++ms')
remMetaVarsCDefn (COpen e (COpenArgs oas))
                 = let (oas',ms1) = unzipWith remMetaVarsOpen oas
                       (e',ms2) = remMetaVars e
                   in (COpen e' (COpenArgs oas'),concat ms1++ms2)


remMetaVarsPackageBody :: CPackageBody -> (CPackageBody,[MetaVar])
remMetaVarsPackageBody (CPackageDef ps pos ds) = 
                  let (ds',ms) = unzipWith remMetaVarsLetDef ds
                  in (CPackageDef ps pos ds',concat ms)
remMetaVarsPackageBody (CPackageInstance e) =
                  let (e',ms) = remMetaVars e
                  in (CPackageInstance e',ms)

remMetaVarsCPatArg :: CPatArg -> (CPatArg,[MetaVar])
remMetaVarsCPatArg (CPatT i e) = let (e',ms) = remMetaVars e
                                 in (CPatT i e',ms)
remMetaVarsCPatArg p = (p,[])

remMetaVarsCPat :: CPat -> (CPat,[MetaVar])
remMetaVarsCPat (CPCon i pas) = 
      let (pas',ms) = unzipWith remMetaVarsCPat pas
      in (CPCon i pas',concat ms)
remMetaVarsCPat (CPVar pa) = let (pas',ms) = remMetaVarsCPatArg pa
                             in (CPVar pas',ms)

remMetaVarsCArg :: CArg -> (CArg,[MetaVar])
remMetaVarsCArg (CArg  is a) = let (a',ms) = remMetaVars a
                               in (CArg is a',ms)

remMetaVarsCSign :: CSign -> (CSign,[MetaVar])
remMetaVarsCSign (CSign i a) = let (a',ms) = remMetaVars a
                               in  (CSign i a',ms)
remMetaVarsCSign (CSignDef d) = let (d',ms) = remMetaVarsCDefn d
                                in (CSignDef d',ms)

remMetaVarsCSummand :: CSummand -> (CSummand,[MetaVar])
remMetaVarsCSummand (i,as) = let (as',ms) = unzipWith remMetaVarsCArg as
                             in ((i,as'),concat ms)

remMetaVarsIndCb (c,es) = let
     (c',m1) = remMetaVarsCSummand c
     (es',m2) = unzipWith remMetaVarsBoolArg es
    in ((c',es'), m1++concat m2)
     

remMetaVarsArm :: (CPat,CExpr) -> ((CPat,CExpr),[MetaVar])
remMetaVarsArm (p,e) = let (p',ms) = remMetaVarsCPat p
                           (e',ms') = remMetaVars e
                       in ((p',e'),ms++ms')

remMetaVarsOpen :: COArg -> (COArg,[MetaVar])
remMetaVarsOpen (COArgT ps x a) = let (a',ms) = remMetaVars a
                                  in (COArgT ps x a',ms)
remMetaVarsOpen (COArgAsT ps n a x) =
                    let (a',ms) = remMetaVars a
                    in (COArgAsT ps n a' x,ms)
remMetaVarsOpen oa = (oa,[])
