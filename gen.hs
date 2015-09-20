-- Translates a list of Haskell examples from a home-made plain-text
-- representation into an html file.

-- Usage:
-- gen < standard.txt > standard.html

main :: IO ()
main = interact go

go :: String -> String
go str = header ++ body str ++ footer

body :: String -> String
body = unlines . map oneLine . lines

oneLine :: String -> String
oneLine "" = "<br>\n"
oneLine "m" = "...\n"
oneLine "t" = "</td><td>\n"
oneLine ('-' : '-' : _) = ""
oneLine ('*' : str) = "\n<h1>" ++ str ++ "</h1>\n"
oneLine ('+' : str) = "\n<h2>" ++ str ++ "</h2>\n"
oneLine ('=' : str) = sp "ex" $
    sp "args" arg1 ++ sp "fn" fn ++ sp "args" arg2 where
  [arg1,fn,arg2] = words str  
oneLine str = sp "ex" $ sp "fn" fn ++ sp "args" argsStr where
  (fn : args) = words str
  argsStr = unwords args

sp :: String -> String -> String
sp tag str = "<span class=\"" ++ tag ++ "\">" ++ str ++ "</span>"

header = "<!DOCTYPE html>\n\
\<html>\n\
\<head>\n\
\  <meta charset=\"UTF-8\">\n\
\  <title>Haskell Examples</title>\n\
\  <link rel=\"stylesheet\" href=\"hs.css\">\n\
\</head>\n<body><table><tr><td>\n"

footer = "\n</td></tr></table></body>\n\
\</html>"
