-- ch05/PrettyStub.hs

-- 빠르게 프로그램을 구현하는 방법으로서, stub을 이용하는 방법을 소개하기 위한
-- 소스코드이다. 프로그램의 윤곽을 stub 을 이용하여 그려놓고 하나씩
-- 구현해나가면서 프로그램을 완성하는 방법이다.

module PrettyStub where

import SimpleJSON

data Doc = ToBeDefined
  deriving (Show)

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

char :: Char -> Doc
char c = undefined

-- hcat concatenates multiple Doc values into one
hcat :: [Doc] -> Doc
hcat xs = undefined

fsep :: [Doc] -> Doc
fsep xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d PrettyStub.<> p) : punctuate p ds