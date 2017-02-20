module InternetLib(
 module URLFetchF,
 module Http,
 module URL,
 module ParseURL,
 encode,decode,encodeQuery,decodeQuery,
 module SimpleHttpServer,
 module Html,
 module HtmlTags,
 module HtmlOps,
 module HtmlParser2,
 module HtmlPrinter,
 getHtml,
 htmlFetchF,
 gifFile,GIFFile,
 pnmFile,PNMFile
 ) where

import URLFetchF
import Http
import URL hiding (Host,Port)
import ParseURL
import URLencode
import URLdecode
import ImageGraphics

import SimpleHttpServer

import Html
import HtmlTags
import HtmlOps
import HtmlPrinter
import HtmlParser2

import Data.Maybe(fromJust)
import Fudgets((>==<),mapFilterSP,absF,mapF)

getHtml :: String -> HttpReq
getHtml urlstr = HttpReq url HttpGet hdrs
  where
   url = fromJust (parseURL urlstr)
   hdrs = [("Accept","text/html")]

htmlFetchF = absF (mapFilterSP post) >==< urlFetchF >==< mapF getHtml
  where
    post (Right (url,Right resp)) = Just (respBody resp)
    post _ = Nothing
