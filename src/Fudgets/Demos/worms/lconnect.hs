module lconnect(lconnect) where
import AllFudgets
import DialogueIO
import Prelude hiding (IOError)

lconnect ports succ =
  case ports
  of []-> abortF (OtherError "failed to open socket") -- could be more informative
     (p:ps)-> openLSocketErrF p (const (lconnect ps succ)) succ

{- obsolete:
openLSocketF' port err succ =
  sIOerr ( (OpenLSocket port)) err
    (\( (LSocket s))-> succ s)
-}

abortF (OtherError s) = error s
abortF _ = error "<<IOError>>"


