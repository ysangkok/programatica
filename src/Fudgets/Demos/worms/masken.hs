module Main where -- fi
import margs
import Fudgets
import connect
import transferSP(decodeSP)
--import IOlml(setCbreak)
import Terminal(setRaw)
import IOUtil(getEnvi)
import ListMap(lookupWithDefault)

--import terminate

user =
  case getEnvi "USER"
  of Just u -> u
     Nothing -> "(unknown)" -- error "getenv: " ++ msg

host = "lips" --default server host

translkey keytab c = [lookupWithDefault keytab c c]

-- CUNBUFF

--termF keytab = translkey keytab>^=<concSP>^^=<startupF [setCbreak] stdioF
termF keytab = translkey keytab>^=<concSP>^^=<stdioF

serverF socket = (serCompSP decodeSP concSP)>^^=<
                     startupF [(user ++ "\n")] (transceiverF socket)

clientF keytab socket = loopF (termF keytab>==<serverF socket)

main = do
    setRaw
    case wargs args host of
       Right (host,keytab)-> fudlogue (connect host [10309,11309] (clientF keytab))
       Left usage-> error usage -- write to stderr instead
