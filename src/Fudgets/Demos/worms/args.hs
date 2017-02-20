module args(wargs) where

wargs argv maxplayers =
  let    usage = "Usage: masken [ players ] [ speed ]\n" ++
	         "        players: 1-" ++ show maxplayers ++ " (number of players)\n" ++
	         "        speed:   25-1000 (milliseconds between ticks)\n"
         default_speed = 200
         default_players = 2
         validspeed s=let speed=read s in 25<=speed && speed<=1000
         validplayer p= let players=read p in 1<=players && players<=maxplayers
  in case argv
   of []-> Right (default_players,default_speed)
      [p,s] | (validplayer p && validspeed s)-> Right (read p,read s)
      [s,p] | (validspeed s && validplayer p)-> Right (read p,read s)
      [p] | (validplayer p)-> Right (read p,default_speed)
      [s] | (validspeed s)-> Right (default_players,read s)
      _ -> Left usage
   {-end-}

{-end-}
