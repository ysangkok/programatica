import HtmlLex

main = interact (sh1 . last . htmlLex)
--main = interact (unlines . map (unwords . lines . show) . htmlLex)
