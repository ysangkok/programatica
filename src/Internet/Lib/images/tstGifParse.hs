import GIF
import GIFparser
import GIFdecompress
import GIF89parser
import DescribeGIF
import System(getArgs)

main =
  do args <- getArgs
     interact (show' args . fmap (interpretGIF89a . decompressGIF) . parseGIF)

show' args =
  if "+v" `elem` args
  then show
  else either id describeGIF

instance Functor (Either e) where
  fmap f (Left x) = Left x
  fmap f (Right y) = Right (f y)
