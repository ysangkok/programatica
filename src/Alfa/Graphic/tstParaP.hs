import Fudgets
import ParagraphP
import ListUtil(chopList,breakAt)

main =
  do s <- getContents
     fudlogue $ shellF "ListLabel" $ mainF s

mainF s = vScrollF (labelF' pm (draw s))

  where pm= setSpacer vCenterS . setSizing Dynamic

draw = vboxD . map (paraD . concatMap (map g .words)) . paras . lines

paraD = PlacedD (paragraphP' (pP 5 0)) . boxD

paras = chopList (breakAt "")
