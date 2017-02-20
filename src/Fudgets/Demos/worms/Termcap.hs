module Termcap where
-- dummy termcap module that assumes an ANSI compatible terminal (vt100, xterm)

clear="\ESC[H\ESC[J"
moveTo x y = "\ESC["++show (y+1)++";"++show (x+1)++"H"
