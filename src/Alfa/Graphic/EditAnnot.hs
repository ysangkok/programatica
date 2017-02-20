module EditAnnot where


class EditAnnot a where
  isSelectable :: a -> Bool
  isHole :: a -> Bool

  redrawFromParent :: a -> Bool
  redrawFromParent = const False

  okRedrawPoint :: a -> Bool
  okRedrawPoint = const True
