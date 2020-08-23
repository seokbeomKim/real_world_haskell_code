module PrettyStub where

import SimpleJSON

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined
