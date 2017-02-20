-- Chinese version of the "Hello, World!" program.
-- Use the -string16bit flag.
import Fudgets

main = fudlogue (shellF "Hello" helloF)

helloF = labelF nihaoD

nihaoD = fontD font (g nihao)

nihao =
  if argFlag "unicode" False
  then "\x4f60\x597d" -- Unicode
  else "\x4463\x3A43" -- GB

font = argKey "font" "-*-fangsong ti-*-*-*-*-16-*-*-*-*-*-gb2312.1980-0"
