{-|

  Used by emacsagda only, not by Alfa.  Supports undo.

-}
module InteractionMonad (module InteractionMonad,module Monads) where
import CITrans
import Monads
import ProofMonad
import ProofState(State)

newtype IM a = IM ([State] -> IO(Error(a,([State]))))


instance  Monad IM  where
 return a        = IM (\s-> return (return (a,s)))
 IM c >>= f = IM (\ss-> do e <- c ss
                           case e of
                              Done (a,ss') -> do let IM g = f a
                                                 g ss'
                              Err msg -> return (Err msg))
     

instance ErrorMonad IM  where
 raise msg  = IM(\s -> return (raise msg))
 
 IM f `handle`  g = IM(\ss -> do e <- (f ss) 
                                 case e of
                                    Done (a,ss') -> return(return(a,ss'))
                                    Err msg -> do let IM g' = g msg
                                                  g' ss)


runIM ::  IM  a -> State -> IO(Error a)
runIM (IM f) s = do e <- f [s]
                    case e of
                        Done (a,_) -> return (return a)
                        Err msg -> return (raise msg)


liftIOIM :: IO a -> IM  a
liftIOIM m = IM(\ss -> (do x <- m
                           return (return (x,ss))) `catch` (\e -> return (internalError (show e))))

liftEIM (Done a) = IM(\ss -> return (return (a,ss)))
liftEIM (Err msg) = raise msg                          

liftPCMIM :: PCM a -> IM a
liftPCMIM (STM f) = IM(\ss -> let e = f (head ss)
                              in  case e of
                                    Done (a,s) -> return (return (a,s:ss))
                                    Err msg -> return (raise msg))

accessPCMIM :: PCM a -> IM a
accessPCMIM (STM f) = IM(\ss -> let e = f (head ss)
                                in  case e of
                                    Done (a,s) -> return (return (a,s:ss))
                                    Err msg -> return (raise msg))

writeIM :: State -> IM ()
writeIM s = IM (\_ -> return (return ((),[s])))

readPCMIM :: PCM a -> IM a
readPCMIM (STM f) = IM(\ss -> let e = f (head ss)
                              in  case e of
                                    Done (a,_) -> return (return (a,ss))
                                    Err msg -> return (raise msg))



--getIMState :: Im a -> IM State
--getImState (Im f) = 


undoIM :: IM ()
undoIM = IM (\ss -> if (length ss) == 1 then return (internalError "Can not undo")
                                  else return (return ((),tail ss)))


nrOfState = IM (\ss -> return (return (length ss - 1,ss)))


undoToIM :: Int -> IM ()
undoToIM i = IM (\ss -> do let k = length ss
                           if i > k then return (internalError "Can not undo")
                                    else return (return ((),drop (k-i-1) ss)))
