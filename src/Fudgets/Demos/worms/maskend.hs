module Main where
import lconnect
import transferSP
import game
import qualified Keys
import args
--import utils
import Fudgets
import socket
import Output(OUTPUT)

main = fudlogue maskend

maskend = stdoutF>==<loopLeftF (game0SP >^^=< worldF)

worldF = serverF >+< startupF [(Just (speed,speed))] timerF
serverF =
    let strip (n,ServerMsg x) = [(n,x)]
        strip _ = []
    in concmapSP strip>^^=<
       socketServer [10309,11309] playerF>=^^<
       broadcastSP [0..players-1]

broadcastSP ns = concmapSP (\x->map (`pair` encode1 x) ns)

playerF n socket peer =
    if n<players
    then --serCompSP (splitAtElemSP (='\n') (const (mapSP SocketMsg)))
	 serCompSP (mapSP SocketMsg)
	           concSP >^^=<
	transceiverF socket
    else absF (putSP SocketEOS nullSP) -- hmm

game0SP = getplayersSP gameSP

gameSP playernames =
    let inp (Left (n,k)) = Keys.Key (n+1) k
        inp (Right _) = Keys.Tick
    in prepostMapSP inp Left (masken players playernames)

getplayersSP contSP = -- not implemented yet
    putSP (Right "Waiting for players to connect:\n")
    (splitAtElemSP splitpt
    (const (putSP (Right "Got enough players: Starting game!\n")
	    (contSP (map (pairwith (("Player "++) . show)) [1..players])))))

splitpt (Left (p,'\n')) = p==players-1
splitpt _ = False

(players,speed) =
  case wargs args maxplayers
  of Right p-> p
     Left usage-> error ("bad args\n" ++ usage)
