module PushButtonF(pushButtonF, pushButtonF', Click(..)) where
import ButtonBorderF
import ButtonGroupF
import CompOps((>=^<))
import Defaults(edgeWidth)
--import Fudget
import SerCompF(idRightF)

data Click = Click  deriving (Eq, Ord, Show)

pushButtonF = pushButtonF' edgeWidth

pushButtonF' edgew keys f =
    buttonGroupF keys (idRightF (buttonBorderF edgew f) >=^< prep)
  where
    toBorder = Left . Left
    toFudget = Left . Right
    through = Right
    prep (Left BMNormal) = toBorder False
    prep (Left BMInverted) = toBorder True
    prep (Left BMClick) = through Click
    prep (Right e) = toFudget e
