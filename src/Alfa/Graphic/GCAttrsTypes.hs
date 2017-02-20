module GCAttrsTypes where
import AllFudgets

class ColorGen a where
  convColorK :: a -> Cont (K i o) Pixel

class FontGen a where
  convFontK :: a -> Cont (K i o) FontStruct

data ColorSpec = (ColorGen ?a) => ColorSpec ?a
data FontSpec = (FontGen ?a) => FontSpec ?a
