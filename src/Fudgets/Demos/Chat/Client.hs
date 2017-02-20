-- Client

import Fudgets
import TypedSockets
import IOUtil(getEnvi)
import Port
import NonStdTrace
import DialogueIO hiding (IOError)

main = fudlogue $ shellF' (setClickToType True) "Chat" top

top = client (argKey "host" "localhost")

client host =
  saver "" >==< menu >+< (tee outF >==< trans (tServerAddress host port))

menufs = [("Save",inputPopupF "Save" oldFilePickF Nothing >=^< 
	                                    const (Just "Append To",Nothing)),
          ("Quit",quitF)]

menu = (snd . snd) >^=< listF menufs >==< 
       marginHVAlignF 0 aLeft aCenter 
       (menuF "File" (map (\(x,y)->((x,()),x)) menufs))

saver f = getF $ \msg ->
	      let same = saver f in
	      case msg of
		 Left new -> saver new
	         Right (SocketMsg msg) -> 
		    if f == "" then same
		    else haskellIOF (AppendFile f (msg ++ "\n")) $ \resp ->
			 case resp of
			    Success -> same
			    Failure e -> trace (show e) (saver "")
	         _ -> same

trans s = map (\(name,msg)->name++" "++msg) >^=< 
          startupF [(I_am username)] (tTransceiverF s) >==< (Msg >^=< inF)

username = case getEnvi "USER" of
		  Just name -> name 
	          _ -> "<anon>"

inF :: F String String
inF = inputDoneSP >^^=< stringF

tee f = filterRightSP >^^=< throughF f

outF :: F (SocketMsg String) Char
outF = 
   let prep (SocketMsg s) = s
       prep SocketEOS = error "The server died!"
   in terminalF defaultFont 20 50 >=^< prep
