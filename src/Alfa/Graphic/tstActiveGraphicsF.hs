import Fudgets
import ActiveGraphicsF
import ParagraphP

main = fudlogue $ shellF "tstActiveGraphicsF" mainF

mainF = outF>==<activeGraphicsF Dynamic True 1 bgColor adrawing >==< inF

outF = shellF "Output" $ vScrollF (displayF' pm) >=^< (draw . show)
  where pm= setSpacer vCenterS . setSizing Dynamic
        draw = paraD . map g . words
        paraD = PlacedD (paragraphP' (pP 5 0)) . boxD

inF = repl adrawing3 >^=< buttonF "Hokus Pokus"
      >*< repl adrawing3b >^=< buttonF "Abra Cadabra"
  where repl =const . Left . replaceGfx [1]
--pre x = Left (replaceAllGfx bla)

bla = undefined :: (ActiveDrawing lbl Void i o)

adrawing =
	spacedD (marginS defaultSep) $
	vboxD [adrawing3b,adrawing1,adrawing2]

adrawing1 =
  tagLeft $
  tableD 3 [
    passiveLeaf "Login:",    activeLeaf stringF,passiveLeaf "visible",
    passiveLeaf "Password:", activeLeaf passwdF,passiveLeaf "hidden"
  ]

adrawing2 =
  tagRight $
  hboxD [activeLeaf (buttonF "Ok"),activeLeaf (buttonF "Cancel")]

adrawing3a =
  tagRight $
  vboxD [passiveLeaf "Not much!",
	 activeLeaf $ buttonF "Boring"]

adrawing3b = spacedD centerS (passiveLeaf "Nothing here!")

adrawing3 =
  tagRight $
  vboxD [passiveLeaf "A calculator!",
	 tableD 4 $ map (activeLeaf . buttonF) "789/456*123-0.=+"]
