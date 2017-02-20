module ReadPic where
import AllFudgets

readPics (file1,file2) cont =
  readPic file1 $ \ p1 ->
  readPic file2 $ \ p2 -> cont (p1,p2)

readPic file cont =
  readBitmapFile file $ \ bmr ->
  case bmr of
    BitmapReturn size _ pm -> cont (pm,size)
    BitmapBad -> error ("Can't read bitmap "++file)

