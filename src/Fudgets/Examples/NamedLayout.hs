module Main where

-- Silly example of use of named layout

import Fudgets

main = fudlogue $ shellF "Fudgets" top


top = nameLayoutF layout $ butt "butt1" "A button" >+< butt "butt2" "Another button" 
    >+< butt "butt3" "A third button"

layout = hBoxNL [vBoxNL [leafNL "butt2", nullNL], vBoxNL [spaceNL (marginS 5) (leafNL "butt3"), leafNL "butt1"]]

butt name label = nameF name $ buttonF label
