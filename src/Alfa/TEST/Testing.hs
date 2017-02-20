module Testing where
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
import UAbstract

default(Int)

myname = "TEST"
maxTest = 51
maxFail = 101


    
showTyp menv m  = (myname++": Show Type Exp",  Message $ Right ("Abstract syntax:\n" ++ show [snd(menv m)] ++ "\n" ++ "Context: \n" ++ concat ["(" ++ show v ++ ", " ++ show t ++ ")\n" |  (v, t) <- fst(menv m)]))

    
qCtx st m = (myname++"Qiao Context",
                 GetPEState$ Message
                 . either (Left .show) (Right .unlines.map show)
                 . (`PE.query` qiaoContext m))
    {-
    testGfx  sy = (myname++"Test Gfx ", GetPEState$ (\st -> DisplayGfx (G$ disp st sy)))
          where
               e = EVar(Var "varialbe...")
               disp st sy =  drawAlfSyntax  st   sy-- draw. EAnnot 
    -}

    
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
                   locenv = reverse (getLocEnv env)
                   result = test1 st m rnd ntest locenv fill_ty [] env

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
                   locenv = reverse (getLocEnv env)
                   result = test1 st m rnd ntest locenv fill_ty [] env
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
                   locenv = reverse (getLocEnv env)
                   result = test1 st m rnd ntest locenv fill_ty [] env


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
    -- --split rnd
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
                   locenv = reverse (getLocEnv env)
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
                   locenv = reverse (getLocEnv env)
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



    shResBdd menv m = (  "BDD: show test result ", GetPEState(\st ->    (showRes st rnd0 1 1)))
       where
       rnd0 =  unsafePerformIO $ newStdGen 
       showRes st rnd ntest nfail 
        | ntest == 5  = Message$ Right$ "Test finished: passed " ++ (show (ntest -1)) ++ " successful tests."
        | nfail == 10  = Message$ Left$ "Arguments exhausted after " ++ (show (ntest -1)) ++ " test cases \n" 
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
                  locenv = reverse (getLocEnv env)
                  result = test2 rnd ntest locenv fill_ty []

                  s = ("Counterexample", EVar (Var "where")): [(na ++ " := ", either (\x -> EVar (Var "Something wrong")) id (PE.query st (computeL m e))) | (na, (e, str)) <- snd result]



  		  test2 rnd ntest  ((name, te, Nothing):ntvs) ty nvs 
      			| hasGen te env  = 
        		  let e1 = genData name te rnd ntest nvs 
      		          in  test2 rnd2 ntest  ntvs ty (nvs ++ [e1])
  		        | condTrue st m te nvs = 
                            let e1 = (name, (trueExp, "True"))
                            in  test2 rnd2 ntest ntvs ty (nvs ++ [e1])
        		| otherwise       = (Nothing,  nvs) 
       			 where
			       (rnd1, rnd2) = next rnd
  		  test2 rnd ntest ((name, te, Just v):ntvs) ty nvs =
        		 let e0 = subst v [(na, ex) | (na, (ex, _)) <- nvs]
             		     e1 = (name, (e0, show e0))
         		 in  test2 rnd2 ntest ntvs ty (nvs ++ [e1])
                       where
          		(rnd1, rnd2) = next rnd
    		test2 rnd ntest  [] (EPi (Var x :- te) te2) nvs
    		  | hasGen te env  = 
  		       let e1 = genData x te rnd ntest nvs
             
        	       in  test2 rnd2 ntest [] te2 (nvs ++ [e1])
      		  | condTrue st m te nvs = 
         		let e1 = (x, (trueExp, "True"))
         		in  test2 rnd2 ntest [] te2 (nvs ++ [e1])
      		  | otherwise       = (Nothing, nvs) 

        	where
       			(rnd1, rnd2) = next rnd
    		test2 rnd ntest [] (EApp (EVar (Var "Taut")) te) nvs   = 
         		let r = subst te [(x, e) | (x,(e,s)) <- nvs]
         		in  case PE.query st (computeL m r) of
                   		Right e1 ->  if bdd e1 == T then (Just True, nvs) else (Just False, nvs)
                   		Left e2 -> (Just False, nvs)
    		test2 rnd ntest [] te nvs   = (Just False, nvs)

    -- --split rnd
{-    genData x te rnd ntest nvs = 
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
-}

-}

