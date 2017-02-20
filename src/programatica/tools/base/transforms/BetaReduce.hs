module BetaReduce where
import HasBaseStruct(hsApp,hsLambda,hsEVar,hsParen)
import Substitute(subst,mapExp)
import HsExpStruct
import HsExpUtil(exposeE)
import HsPatUtil(isPVar)
import Maybe(fromMaybe)

{-+
Find outermost beta redexes and reduce them.
Beta redexes are expressions of the form

  (\ x1 ... xn -> e) e1 ... en.

Pattern matching is not implemented.
-}

beta e =
    case isLambda fun of
      Just (ps, body) -> substArgs [] ps args body
      _ -> mapExp beta e
  where
    (fun,args) = flatApp e []

    substArgs s (p:ps) (arg:args) body =
      case isPVar p of
        Just x -> substArgs ((x,arg):s) ps args body
        _ -> keep s (p:ps) (arg:args) body
    substArgs s ps args body = keep s ps args body

    keep s ps args body = pApps (hsLambda' ps (subst f body)) args
      where f y = fromMaybe (hsEVar y) (lookup y s)

    
    flatApp e args =
      case exposeE e of
        Just (HsApp fun arg) -> flatApp fun (arg:args)
        --Just (HsInfixApp e1 i e2) -> can not be a beta redex
        _ -> (e,args)

    pApps fun [] = fun
    pApps fun args = apps (hsParen fun) args

    apps fun [] = fun
    apps fun (arg:args) = apps (hsApp fun arg) args

isLambda e =
  case exposeE e of
    Just (HsLambda ps body) -> Just (ps,body)
    _ -> Nothing

-- Don't create arity 0 lambda asbtractions.
hsLambda' [] e = e
hsLambda' ps e = hsLambda ps e
