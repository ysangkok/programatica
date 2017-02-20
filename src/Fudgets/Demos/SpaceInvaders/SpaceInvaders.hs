module Main where
import Fudgets
import CanonF
import BulletF
import SpaceF
import InvadersF
import ScoreF

main = fudlogue (shellF "Space Invaders" spaceInvadersF)

spaceInvadersF = (panelF,RightOf)>#==<spaceF (Point 480 390) objectsF

panelF = (quitButtonF,Below)>#+<scoreF>=^<Right

objectsF space = invadersF space >==< bulletF >==< canonF space >=^< stripEither
