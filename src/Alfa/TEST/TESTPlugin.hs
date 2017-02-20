module TESTPlugin where
import AlfaPluginKit
import FileConv(prSyntax)
import Char(isSpace)
import qualified ProofEngine as PE
--import Debug2(trace)
import Agda(metaType, metaContext)
import AgdaProofEngine(liftPCM)
import qualified ConvFromAgda as FAg 
import Random
import TestData
--import IOExts
import TESTLazy(computeL, qiaoContext)
--import DrawAlf
import Fud --AlfOps  hiding (AlfEditMsg(..))
--import AlfOps hiding (AlfEditMsg(..))
--import Testing
import BddTable
import UAbstract
import BooleanAlToBDD (expisBDD1, ex2cm)
default(Int)

myname = "TEST"
maxTest = 51
maxFail = 101

plugin =
  Methods {
    name = myname,
    state0 = \ _ -> return (0::Int),
    loadDecls =
      \ dsrc s _ ->
      if isImported dsrc -- hmm
      then const (Right s)
      else Right . (+1) . readCnt . fst . head . lex . concat . map snd,
    save = \ st -> [show st ++ " Test plugin was here!"],
    describe = descr,
    altdisp = \ st -> [("Plain",showAsPlain)],
    altparse = \ st -> [],
    fileparsers = [("txt",txtparser)]
  }
  where
    readCnt s = case reads s of
		  (n,_):_ -> n
		  _ -> 0

    txtparser st imported filename contents = Right (Module [cmnt],st)
      where
        cmnt = Comment ("{-\n"++contents++"\n-}")

    descr st menv (sy:ancestors) =
      --trace (unlines . map prSyntax $ sy:ancestors) $
      case sy of
        ModuleS (Module ds) ->
	    (Just (myname++" plugin is awake! "++show st), [])

	ExpS e ->
          case e of
	    EAnnot (AsTextBy _) _ -> (Nothing,[])
            EMeta m  -> (Just "A goal is selected",   [ checkShowRes menv m, check  menv m, checkShow2 menv m, check2 menv m, checkShow  menv m,  shResBdd menv m, debugShow menv m, showTyp menv m,  qCtx  m,  qCtxLoc  menv m])
	    _ -> (Nothing, [("Show as plain text",showAsPlainCmd e),
			   ("Show in the graphics window",displaySyntax sy)])                   --(Just "An expression is marked", [showAsyntMenu [e]])

	DeclS d -> (Just "DeclS marked", [(myname ++ "show marked", Message $ Right (show d))]) --(Nothing, [])
	DeclsS ds -> (Nothing,[])
        _ -> (Nothing,[])




    showTyp menv m  = (myname++": Show Type Exp",  Message $ Right ("Abstract syntax:\n" ++ show [snd(menv m)] ++ "\n" ++ "Context: \n" ++ concat ["(" ++ show v ++ ", " ++ show t ++ ")\n" |  (v, t) <- fst(menv m)]))

    qCtx  m = (myname++"Qiao Context",
                 GetPEState$ Message
                 . either (Left .show) (Right .unlines.map show)
                 . (`PE.query` qiaoContext m))

    qCtxLoc  menv m = (myname++"Qiao Context",
                 GetPEState (\st -> Message$  Right$  show (typexp st)))
         where
                    env st = case PE.query st (qiaoContext m) of
                               Left _ -> []
                               Right e -> e
                    locenv st = reverse (getLocEnv (env st))
                    menvm = menv m  
                    typexp st = fullTyp (snd menvm)(env st)

    checkShowRes menv m = (myname ++ ": show test result using domain-based generators ", GetPEState(\st ->    (showRes st rnd0 1 1)))
       where
       rnd0 =  unsafePerformIO $ newStdGen 
       showRes st rnd ntest nfail 
        | ntest == maxTest  = Message$ Right$ "Test finished: passed " ++ (show (ntest -1)) ++ " successful tests."
        | nfail == maxFail  = Message$ Left$ "Arguments exhausted after " ++ (show (ntest -1)) ++ " test cases \n" 
        | otherwise         = 
            case fst result of
                 Nothing    -> showRes st rnd2 ntest (nfail+1)
                 Just True  ->  showRes st rnd2 (ntest+1) nfail
                 Just False -> displaySyntax2 s
             -- Message$ Left$ ppRes result ntest 

             where 
                   (rnd1, rnd2) = next rnd
                   fill_ty = snd(menv m)
                   env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                   locenv = [] --reverse (getLocEnv env)
                   typexp = fullTyp fill_ty env -- full type expression as a Pi type
                   result = test1 st m rnd ntest locenv typexp [] env

                   s = ("Counterexample", EVar (Var "where")): [(na ++ " := ", either (\x -> EVar (Var "Something wrong")) id (PE.query st (computeL m e))) | (na, (e, str)) <- snd result]


                  --EStr$  [decl' [defA$ Binding (Var "Result", EVar (Var "False, where "))], decl' [defA$  Binding (Var na, e) | (na, (e, str)) <- snd result]]
    checkShow menv m = (myname ++ ": show test data and result using domain-based generators ", GetPEState(\st -> displaySyntax2 (showRes st rnd0 1 1 [])))
       where
       rnd0 =  unsafePerformIO $ newStdGen 
       showRes st rnd ntest nfail syn
        | ntest == maxTest  =  ("Test finished: passed ", EVar (Var$ (show (ntest -1)) ++ " successful tests.")):("Test data: variable name and ", EVar (Var "value")):syn
        | nfail == maxFail  = ("Arguments exhausted after ", EVar (Var$ (show (ntest -1)) ++ " test cases.")):("Test data: variable name and ", EVar (Var "value")):syn
        | otherwise         = 
            case fst result of
                 Nothing    ->  showRes st rnd2 ntest (nfail+1) (syn ++ s ntest result "Condition failed")  
                 Just True  -> showRes st rnd2 (ntest+1) nfail (syn ++ s ntest result "True") 
                 Just False -> showRes st rnd2 ntest (nfail+1) (syn ++ s ntest result "False") 

             where 
                   (rnd1, rnd2) = next rnd
                   fill_ty = snd(menv m)
                   env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                   locenv = [] --reverse (getLocEnv env)
                   typexp = fullTyp fill_ty env
                   result = test1 st m rnd ntest locenv typexp [] env
                   s n res res' = ("Result = " ++ res' ,  EVar (Var ("(" ++ show n ++ ")"))): [(na ++" := ", either (\x -> EVar (Var "Something wrong")) id (PE.query st (computeL m e))) | (na, (e, str)) <- snd res]

    ppRes (Nothing, nvs) ntest = "test data: \n"  ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nvs] ++ "Result:  Conditon failed (n = " ++ show ntest   ++ ")\n\n"
    ppRes (Just res, nvs)  ntest = "Testing failed: \n test data: \n"  ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nvs] ++ "Result: " ++ show res ++" (n = " ++ show ntest   ++ ")\n\n"

    check menv m = (myname ++ ": solve goal using domain-based generators ", GetPEState(\st ->  showRes st rnd0 1 1))
     
     where
       rnd0 =  unsafePerformIO $ newStdGen 
       showRes  st rnd ntest nfail 
        | ntest == maxTest  =  Give (tested fill_ty)
        | nfail == maxFail  = Message$ Right$ "Arguments exhausted after " ++ (show (ntest -1)) ++ " test cases \n" 
        | otherwise         = 
            case fst result of
                 Nothing    -> showRes st rnd2 ntest (nfail+1)
                 Just True  -> showRes st rnd2 (ntest+1) nfail
                 Just False -> Message$ Right$ ppRes result ntest

             where
                   (rnd1, rnd2) = next rnd
                   fill_ty = snd(menv m)

                   env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                   locenv = [] -- reverse (getLocEnv env)
                   typexp = fullTyp fill_ty env 
                   result = test1 st m rnd ntest locenv typexp [] env

    test1 st m rnd ntest  [] (EPi (Var x :- te) te2) nvs env
      | hasGen te env  = 
         let e1 = genData x te rnd ntest nvs
             
         in  test1 st m rnd2 ntest [] te2 (nvs ++ [e1]) env
      | condTrue st m te nvs = 
         let e1 = (x, (trueExp, "True"))
         in  test1 st m rnd2 ntest [] te2 (nvs ++ [e1]) env
      | otherwise       = (Nothing, nvs) 

        where
       (rnd1, rnd2) = next rnd
    test1 st m rnd ntest [] te nvs env   = 
         let r = subst te [(x, e) | (x,(e,s)) <- nvs]
         in  case PE.query st (computeL m r) of
                   Right e1 ->  if isUnit' e1 then (Just True, nvs) else (Just False, nvs)
                   Left e2 -> (Just False, nvs)
{-
    test1 st m rnd ntest  ((name, te, Nothing):ntvs) ty nvs env 
      | hasGen te env  = 
         let e1 = genData name te rnd ntest nvs 
         in  test1 st m rnd2 ntest  ntvs ty (nvs ++ [e1]) env
      | condTrue st m te nvs = 
         let e1 = (name, (trueExp, "True"))
         in  test1 st m rnd2 ntest ntvs ty (nvs ++ [e1]) env
      | otherwise       = (Nothing,  nvs) 
        where
       (rnd1, rnd2) = next rnd
    test1 st m rnd ntest ((name, te, Just v):ntvs) ty nvs env =
         let e0 = subst v [(na, ex) | (na, (ex, _)) <- nvs]
             e1 = (name, (e0, show e0))
         in  test1 st m rnd2 ntest ntvs ty (nvs ++ [e1]) env
           where
          (rnd1, rnd2) = next rnd
-}
    genData x te rnd ntest nvs = 
         let
             n' = ntest `div` 2 + 3
             n1 = n' `mod` 13
             (e0, estr) = typeExp2Data te rnd n1
             e1' = subst e0 [(na, ex) | (na, (ex, _)) <- nvs]
         in   (x, (e1', estr)) 
    condTrue st m te nvs = 
          let r = subst te [(x, e) | (x,(e,s)) <- nvs]  
          in  case PE.query st (computeL m r) of
                     Right e1 ->  if isUnit' e1 then True else False
                     Left e2 -> False 

    isUnit (ESum [Constr (Con "tt", (Ctx Nothing [], []))]) = True
    isUnit _ = False             
    tested e = EApp (EVar (Var "Tested")) e  
    is_true (ECon (Con "true")) = True
    is_true _ = False
    isUnit' e = (isUnit e) || (is_true e)
    isFalse (ESum []) = True
    isFalse _ = False
    trueExp = ECon (Con "tt")



    checkShow2 menv m = (myname ++ ": show test result using special-purpose generator", GetPEState(\st -> (showRes st rnd0 1 1)))
     where
       rnd0 =  unsafePerformIO $ newStdGen -- mkStdGen 1
       fill_ty = snd(menv m)
       ty_exp = recover (menv m)
       ts = getTypings ty_exp
       genname = "rand_" ++ (lookUp_pn (fst(menv m)))
       showRes st rnd ntest nfail 
        | ntest == maxTest  =  Message $ Right$ "Test finished: passed " ++ (show (ntest -1)) ++ " successful tests."
        | nfail == maxFail  =  Message $ Left$ "Arguments exhausted after" ++ (show (ntest -1)) ++  " test cases. \n"
        | otherwise     =
            case fst result of
                 Nothing    ->  showRes st rnd2 ntest (nfail+1)
                 Just True  -> showRes st rnd2 (ntest+1) nfail
                 Just False -> displaySyntax2 cex
                     -- ppRes result ntest ++ showRes st rnd2 ntest (nfail+1)

             where 
                   (rnd1, rnd2) = next rnd
                   sz = (ntest `div` 2) + 3
                   e0 = gen_with_name genname rnd sz
                   (e1, nvs) = case PE.query st (computeL m e0)  of
                                Right e ->  
                                  let
                                     es = flatStr e 
                                     e' = genExp' ty_exp [ e1 | (_, e1) <- es]
                                  in (e', [(x, (e1, show e1))| (Var x, e1) <- es])
                                Left _ -> (ESum [], [])
                   env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                   locenv = [] -- reverse (getLocEnv env)
                   typexp = fullTyp fill_ty env
                   result = test1 st m rnd ntest [] e1 nvs env
                   cex = [(na, either (\x -> EVar (Var "Something wrong")) id (PE.query st (computeL m e))) | (na, (e, str)) <- snd result]

    check2 menv m = (myname ++ ": solve goal using special-purpose generator", 
                           GetPEState(\st -> showRes st rnd0 1 1)) 
      where 
        rnd0 = unsafePerformIO $ newStdGen --mkStdGen 1
        fill_ty = snd(menv m)
        ty_exp = recover (menv m)
        genname = "rand_" ++ (lookUp_pn (fst(menv m)))
        showRes st rnd ntest nfail
         | ntest == maxTest = Give (tested fill_ty)
         | nfail == maxFail = Message$ Left$  "Arguments exhausted after " ++ (show (ntest -1)) ++ " test cases \n" 
         | otherwise     = 
            case fst result of
                 Nothing    -> showRes st rnd2 ntest (nfail+1)
                 Just True  -> showRes st rnd2 (ntest+1) nfail
                 Just False -> Message$ Right$ ppRes result ntest

             where 
                   (rnd1, rnd2) = next rnd
                   sz = (ntest `div` 2) + 3
                   e0 = gen_with_name genname rnd sz
                   (e1, nvs) = case PE.query st (computeL m e0)  of
                                Right e ->  
                                  let
                                     es = flatStr e 
                                     e' = genExp' ty_exp [ e1 | (_, e1) <- es]
                                  in (e', [(x, (e1, show e1))| (Var x, e1) <- es])
                                Left _ -> (ESum [], [])
                   env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                   locenv = [] --reverse (getLocEnv env)
                   typexp = fullTyp fill_ty env
                   result = test1 st m rnd ntest [] e1 nvs env


    showAsyntMenu ds =
          (myname++": Show abstract syntax", Message $ Right ("Abstract syntax:\n" ++ show ds))


    showAsPlainCmd = Replace . syn . EAnnot (asTextBy (myname,"Plain"))

    showAsPlain menv = map showPara . lines . prSyntax
      where
        showPara s =
          case span isSpace s of
            (sp,txt) ->
		--(length sp,map TPlain $ words txt)
                PlainPara (map TPlain $ words txt)



  
    debugShow menv m = (myname ++ "Debug: show test data and result", GetPEState(\st ->  Message $ Right (showRes st rnd0 0 0)))
     where
       rnd0 =  unsafePerformIO $ newStdGen -- mkStdGen 1
       fill_ty = snd(menv m)
       ty_exp = recover (menv m)
       ts = getTypings ty_exp
       showRes st rnd ntest nfail 
        | ntest == maxTest  = "Test finished"
        | nfail == maxFail  = "Arguments exhausted after" ++ (show ntest) ++ "\n"
        | otherwise     = " ntest: " ++ (show ntest) ++ "\n" ++ " nfail: " ++ (show nfail) ++ (test1 st rnd ntest nfail ty_exp [])
       test1 st rnd ntest nfail te nes = 
           case te of 
{-             (EPi (Var x :- (EApp (EVar (Var "T")) te1)) te2) -> 
                  let r = subst te1 [(na, ex) | (na, (ex, _)) <- nes]
                      str = "test data:  "  ++"\n" ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nes] ++ "\n" 
                  in case PE.query st (computeL m r) of
                       Right e1 ->  ( if isUnit' e1 then test1 st rnd2 ntest nfail te2 (nes ++ [(x, (ECon (Con "tt"), show (ECon (Con "tt"))))]) else showRes st rnd2 ntest (nfail + 1))
                       Left e2 -> show ntest ++ ": testing failed" ++ e2
-}
             (EPi (Var x :- te1) te2) ->  if (hasGen te1 env) then
                let
                   n' = ntest `div` 2 + 3
                   n1 = n' `mod` 13
                   (e0, estr) = typeExp2Data te1 rnd n1
                   e1' = subst e0 [(na, ex) | (na, (ex, _)) <- nes]
                   e1 = (x, (e1', estr)) 
                 --  e1 = (x, typeExp2Data te1 rnd n1)
                in test1 st rnd2 ntest nfail te2 (nes ++ [e1])
              else
                  let r = subst te1 [(na, ex) | (na, (ex, _)) <- nes]
                      str = "test data:  "  ++"\n" ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nes] ++ "\n" 
                  in case PE.query st (computeL m r) of
                       Right e1 ->  ( if isUnit' e1 then test1 st rnd2 ntest nfail te2 (nes ++ [(x, (ECon (Con "tt"), show (ECon (Con "tt"))))]) else showRes st rnd2 ntest (nfail + 1))
                       Left e2 -> show ntest ++ ": testing failed" ++ e2

             _ -> 
                 let                
                   es = [e1 | (n, (e1, s))  <- nes]
                   e' = genExp' ty_exp es
                   str =  "test data:  "  ++"\n" ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nes] ++ "\n"  
                 in   case PE.query st (computeL m e') of
                        Right e1 -> {-show n ++ ": testing\n" ++ "test data:  "  ++"\n" ++ concat [x ++ " := "  ++ s ++ "\n" | (x, (e, s)) <- nes] ++ "\n" ++ "genExp: \n" ++ show e' ++"\n" ++-} str ++ "Result:(n =" ++ show ntest ++(')': show e1) ++ "\n" ++ (showRes st rnd2 (ntest+1) nfail)

                        Left e2   -> show ntest ++ ": testing failed" ++ e2

          where
               (rnd1, rnd2) = next rnd --split rnd
               env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e



    shResBdd menv m = (  "BDD: show BDD check result ", GetPEState(\st ->    (showRes st rnd0 1 1)))
       where
       rnd0 =  unsafePerformIO $ newStdGen 
       showRes st rnd ntest nfail 
        | ntest == 6  = Message$ Right$ "BDD check finished: passed " ++ (show (ntest -1)) ++ " successful tests."
        | nfail == 11  = Message$ Left$ "Arguments exhausted after " ++ (show (ntest -1)) ++ " test cases \n" 
        | otherwise         = 
            case pro31 result of
                 Nothing    -> showRes st rnd2 ntest (nfail+1)
                 Just True  ->  showRes st rnd2 (ntest+1) nfail
                 Just False -> displaySyntax2 s
             -- Message$ Left$ ppRes result ntest 

             where 
                  (rnd1, rnd2) = next rnd
                  fill_ty = snd(menv m)
                  env = case PE.query st (qiaoContext m) of
                           Left _ -> []
                           Right e -> e
                  locenv = [] --reverse (getLocEnv env)
                  typexp = fullTyp fill_ty env
                  result = test2 rnd ntest locenv typexp []

                  s = ("Counterexample", EVar (Var "where")): [(na ++ " := ", either (\x -> EVar (Var "Something wrong")) id (PE.query st (computeL m e))) | (na, (e, str)) <- pro32 result] ++ [("assignments ", EVar (Var cms))]
                  cms = pro33 result

                  pro31 (x,y,z) = x
                  pro32 (x,y,z) = y
                  pro33 (x,y,z) = z


    		  test2 rnd ntest  [] (EPi (Var x :- te) te2) nvs
    		    | hasGen te env  = 
  		         let e1 = genData x te rnd ntest nvs
             
        	         in  test2 rnd2 ntest [] te2 (nvs ++ [e1])
      		    | condTrue st m te nvs = 
         		let e1 = (x, (trueExp, "True"))
         		in  test2 rnd2 ntest [] te2 (nvs ++ [e1])
      		    | otherwise       = (Nothing, nvs,[]) 

        	   where
       			(rnd1, rnd2) = next rnd
    		  test2 rnd ntest [] (EApp (EVar (Var "Taut")) te) nvs   = 
         		let r = subst te [(x, e) | (x,(e,s)) <- nvs]
         		in  case PE.query st (computeL m r) of
                   		Right e1 ->  if expisBDD1 e1 then (Just True, nvs, []) else (Just False, nvs, ex2cm  e1)
                   		Left e2 -> (Just False, nvs,[])
{-
  		  test2 rnd ntest  ((name, te, Nothing):ntvs) ty nvs 
      			| hasGen te env  = 
        		  let e1 = genData name te rnd ntest nvs 
      		          in  test2 rnd2 ntest  ntvs ty (nvs ++ [e1])
  		        | condTrue st m te nvs = 
                            let e1 = (name, (trueExp, "True"))
                            in  test2 rnd2 ntest ntvs ty (nvs ++ [e1])
        		| otherwise       = (Nothing,  nvs, []) 
       			 where
			       (rnd1, rnd2) = next rnd
  		  test2 rnd ntest ((name, te, Just v):ntvs) ty nvs =
        		 let e0 = subst v [(na, ex) | (na, (ex, _)) <- nvs]
             		     e1 = (name, (e0, show e0))
         		 in  test2 rnd2 ntest ntvs ty (nvs ++ [e1])
                       where
          		(rnd1, rnd2) = next rnd
-}
    		  test2 rnd ntest [] te nvs   = (Just False, nvs,[])




