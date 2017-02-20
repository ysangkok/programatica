module connect(connect) where
import AllFudgets
import DialogueIO
import Prelude hiding (IOError)

connect host ports succ =
  case ports
  of []-> abortF (OtherError "failed to open socket") -- could be more informative
     (p:ps)-> openSocketErrF host p (const (connect host ps succ)) succ
  {-end-}

{- obsolete:
openSocketF' host port err succ =
  sIOerr ((OpenSocket host port)) err
    (\( (Socket s))-> succ s)
-}

abortF (OtherError s) = error s
abortF _ = error "<<IOError>>"

{-end-}
