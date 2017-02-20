module Main where -- masken1 -- standalone version
import Output
import game
import args
import itag
import qualified Keys
import io
--import IOlml(setCbreak)
import Terminal(setRaw)
import Fudgets

main = do
  setRaw
  case wargs args maxplayers of
     Right (players,speed) ->
       let playernames = map (\n->(n,"Player " ++ show n)) ([1..players])
           tickF = const Keys.Tick>^=<startupF [(Just (speed,speed))] timerF
           game_inputF  = stripEither>^=<(tag_input>^=<(concSP>^^=<stdinF) >+< tickF)
           --game_outputF = startupF [setCbreak] stdoutF>=^<print'
           game_outputF = stdoutF>=^<print'
           gameSP = masken players playernames
       in fudlogue (game_outputF>=^^<gameSP>==<game_inputF)
     Left usage-> error ("bad args\n"++usage) -- error!
