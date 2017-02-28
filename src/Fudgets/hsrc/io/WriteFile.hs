module WriteFile where
import HaskellIO(haskellIOF)
--import CompOps((>^=<))
import Cont(contMap)
--import NullF
import DialogueIO hiding (IOError)

writeFileF = contMap wr
    where
      wr (file,contents) cont =
        haskellIOF (WriteFile file contents) $ \ resp ->
	cont (file,post resp)

      post (Failure e) = Left e
      post Success = Right ()
