module MimeTypes where
import Data.ListUtil(chopList,breakAt)
import MimeMessage(Headers)

typeHdrs path = encHdr (extensions path)
  where
    encHdr es =
      case es of
        "gz":es -> gzipEncoding:typeHdr es
        "Z":es ->  encodingType "x-compress":typeHdr es
	_ -> typeHdr es

    typeHdr es =
      case es of
	"html":_ -> htmlType
	"htm":_  -> htmlType
	"xml":_  -> xmlType
	"txt":_  -> txtType
	"gif":_  -> gifType
	"jpg":_  -> jpegType
	"png":_  -> pngType
        "pdf":_  -> pdfType
        "ps":_   -> psType
        "mp2":_  -> mpegAudioType
        "mp3":_  -> mpegAudioType
        "mpg":_  -> mpegVideoType
        "mpeg":_ -> mpegVideoType
        "avi":_  -> contentType "application/x-msvideo"
	_        -> contentType "application/octet-stream"

gzipEncoding = encodingType "x-gzip"
htmlType = contentType "text/html"
xmlType = contentType "text/xml"
txtType = contentType "text/plain"
gifType = contentType "image/gif"
jpegType = contentType "image/jpeg"
pngType = contentType "image/png"
pdfType = contentType "application/pdf"
psType = contentType "application/postscript"
mpegAudioType = contentType "audio/mpeg"
mpegVideoType = contentType "mpeg/video"

encodingType e = ("Content-encoding",e)
contentType t = [("Content-type",t)]::Headers

extensions "" = []
extensions path =
  reverse . drop 1 . chopList (breakAt '.') . head .
  reverse . chopList (breakAt '/') $
  path
