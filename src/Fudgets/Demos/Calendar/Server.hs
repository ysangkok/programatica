-- Server

import Fudgets
import TypedSockets
import Port
import UnsafeDirty(force) -- japp!

main = fudlogue (server port)

server port =
    loopF (databaseSP [] [] >^^=< tSocketServerF port clienthandler)
  where
    clienthandler peer transceiver = transceiver

    databaseSP cl db = 
	 getSP $ \(i,msg) -> -- A message from client number i:
	 let clbuti = filter (/= i) cl
	 in case msg of
	      -- A new client, send the database to it,
	      -- and add to client list:
	      ClientNew -> putsSP [(i,d) | d <- db] $
			   databaseSP (i:cl) db
	      -- Update entry in the database...
	      ClientMsg s -> let db' = replace s db
			     in seq (force db') $
				-- ... and tell the other clients
				putsSP [(i',s) | i' <- clbuti] $
				databaseSP cl db'
	      -- A client disconnected, remove it from 
	      -- the client list:
	      ClientEOS -> databaseSP clbuti db
