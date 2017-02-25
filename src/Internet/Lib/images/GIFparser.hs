{-# LANGUAGE CPP #-}
module GIFparser(parseGIF,sizeOfGIF) where
import GIF
import ParsOps2
import Utils2Janus(bit,bits)

default(Int)

#define map fmap

--import Trace
--tr s = trace s $ return ()
tr s = return ()

sizeOfGIF = parse gifSizeP
  where
    gifSizeP =
      do signatureP
         sd <- screenDescriptorP
	 theRest
	 return (swidth sd,sheight sd)

parseGIF = showErr . parse gifP

showErr = either (Left . sh) Right
  where
    sh (msg,s) = msg++": ("++show (length s)++") "++take 80 s

gifP =
   do h <- signatureP
      sd <- screenDescriptorP
      gmap <- optColorMapP (hasGlobalMap sd) (sbitsPerPixel sd)
      ds <- dataBlocksP
      lit ';'
      theRest -- allow trailing garbage
      return (GIF h sd gmap ds)

signatureP = lits "GIF87a" `orelse` lits "GIF89a"

optColorMapP b n =
  if b
  then map Just (colorMapP n)
  else tr "no colormap" >> return Nothing

colorMapP size = tr ("colorMapP "++show n) >> repeatP n colorP
  where n = 2^size

colorP =
  do r <- byteP
     g <- byteP
     b <- byteP
     return (r,g,b)

screenDescriptorP =
  do w <- wordP
     h <- wordP
     opts <- byteP
     bg <- byteP
     ar <- byteP
     return (SD w h (bit 7 opts) (bitcount 4 opts) (bit 3 opts) (bitcount 0 opts) bg ar)

bitcount p byte = 1+bits p 3 byte

dataBlocksP = many dataBlockP

dataBlockP = eitherP extensionBlockP imageP

imageP = 
  do lit ','
     id <- imageDescriptorP 
     lmap <- optColorMapP (hasLocalMap id) (ibitsPerPixel id)
     rd <- rasterDataP
     return (Image id lmap rd)

imageDescriptorP =
  do l<-wordP
     t<-wordP
     w<-wordP
     h<-wordP
     opts<-byteP
     return (ID l t w h (bit 7 opts) (bit 6 opts) (bitcount 0 opts))
  
rasterDataP = map Left compressedBlocksP

compressedBlocksP =
  do c <- byteP
     bs <- blocksP
     return (CB c bs)

extensionBlockP =
  do lit '!'
     c <- byteP
     bs <- blocksP
     return (EB c bs)

blocksP =
  do cnt <- byteP
     if cnt == 0
      then return []
      else do bytes <- bytesP cnt
              blocks <- blocksP
	      return (bytes:blocks)

bytesP :: Int -> Parser Char [Byte]
--bytesP n = repeatP n byteP
bytesP n = map (map fromEnum) (tokens n)
wordsP n = repeatP n wordP

byteP :: Parser Char Byte
byteP = map fromEnum token

wordP = 
  do lo <- byteP
     hi <- byteP
     return (hi*256+lo)
