module DialogueIO(Request(..), Response(..), IOError(..)
       , Dialogue(..), SigAct(..) , dialogueToIO
       --, module _LibDialogue
	) where
import Prelude hiding (IOError)
import P_IO_data

dialogueToIO :: Dialogue -> IO ()
dialogueToIO = error "DialogueIO.dialogueToIO"
