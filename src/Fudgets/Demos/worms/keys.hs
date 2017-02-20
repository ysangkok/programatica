module keys where


data INPUT
  = Tick			-- hiatons
  | Key Int Char		-- Key userno key

upkey = 'i'
leftkey = 'j'
rightkey = 'l'
downkey = 'm'
turnleftkey = ','
turnrightkey = '.'

stdkeys = [upkey,leftkey,rightkey,downkey,turnleftkey,turnrightkey]

show_INPUT Tick = "Tick"
show_INPUT (Key n c) = "(" ++ show n ++ "," ++ [c] ++ ")"

{-end-}
