import Fudgets
import ImageGraphics(gifFile)

main = fudlogue $ shellF "Gif Viewer" $ gifsF args

gifsF = untaggedListF . map (labelF . gifFile)
