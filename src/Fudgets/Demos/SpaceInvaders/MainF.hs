module MainF where
import Fudgets
import ScoreF

mainF worldF = nameLayoutF layout spaceInvadersF
  where
    layout = hBoxNL [w,vBoxNL [a s,a p,q]]
      where
        [w,p,q,s] = map leafNL ["w","p","q","s"]
	a = marginHVAlignNL 0 aCenter aTop

    spaceInvadersF = outF>==<nameF "w" (worldF (Point 480 390))>==<inF

    inF = nameF "p" (toggleButtonF "Pause")

    outF = (nameF "q" quitButtonF>+<nameF "s" scoreF)>=^<Right
