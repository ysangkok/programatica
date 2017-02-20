module ScoreF where
import Fudgets

scoreF = marginHVAlignF 0 aLeft aTop
         (((highF,Below)>#+<currentF)>==<toBothF>=^^<sumSP)

highF = dispF "High" >=^^< maxSP
currentF = dispF "Score"

sumSP = accSP (+) 0
maxSP = accSP max 0

accSP f = mapstateSP (\s x->let x'=f s x in (x',[x']))

dispF lbl = lbl `labAboveF` intDispF
