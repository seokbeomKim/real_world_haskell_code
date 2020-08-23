module Prettify where

import PrettyStub

data Doc
  = Empty
  | Char char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

emtpy :: Doc

empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

-- Line constructor represents a line break
line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d : ds) =
      case d of
        Empty -> transform ds
        Char c -> c : transform ds
        Text s -> s ++ transform ds
        Line -> '\n' : transform ds
        a `Concat` b -> transform (a : b : ds)
        _ `Union` b -> transform (b : ds)
