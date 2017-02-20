import PNMparser
import PNM2GIF
import GIFprinter

main = interact (either error (printGIF . pnm2gif) . parsePNM)
