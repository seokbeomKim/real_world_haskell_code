-- EAN-13 바코드 해석 코드

import           Control.Applicative            ( (<$>) )
import           Data.Array                     ( Array(..)
                                                , bounds
                                                , elems
                                                , indices
                                                , ixmap
                                                , listArray
                                                , (!)
                                                )
import qualified Data.ByteString.Lazy.Char8    as L
import           Data.Char                      ( digitToInt )
import           Data.Ix                        ( Ix(..) )
import           Data.List                      ( foldl'
                                                , group
                                                , sort
                                                , sortBy
                                                , tails
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                )
import           Data.Ratio                     ( Ratio )
import           Data.Word                      ( Word8 )
import qualified Parse                         as P
                                                ( Parse
                                                , assert
                                                , identity
                                                , parseByte
                                                , parseNat
                                                , parseWhileWith
                                                , skipSpaces
                                                , w2c
                                                , (==>)
                                                , (==>&)
                                                )

import           Data.Function                  ( on )
import           System.Directory
import           System.Environment             ( getArgs )

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (sum products `mod` 10)
  where products = mapEveryOther (* 3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f, id])

leftOddList =
  [ "0001101"
  , "0011001"
  , "0010011"
  , "0111101"
  , "0100011"
  , "0110001"
  , "0101111"
  , "0111011"
  , "0110111"
  , "0001011"
  ]

-- leftOddList는 [ [Char] ] 이므로, [Char] 에 대해 complement를 fmap
-- 해준 후에 이를 다시 map으로 해줘야 한다.
rightList = map complement <$> leftOddList
 where
  complement '0' = '1'
  complement '1' = '0'

leftEvenList = map reverse rightList

parityList =
  [ "111111"
  , "110100"
  , "110010"
  , "110001"
  , "101100"
  , "100110"
  , "100011"
  , "101010"
  , "101001"
  , "100101"
  ]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l - 1) xs where l = length xs

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList

foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a

-- indices 함수는 Array a가 가지고 있는 인덱스들을 반환한다.
foldA f s a = go s (indices a)
 where
  go s (j : js) = let s' = f s (a ! j) in s' `seq` go s' js
  go s _        = s

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first : rest) =
  outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
 where
  (left, right) = splitAt 5 rest
  lefties       = zipWith leftEncode (parityCodes ! first) left
  righties      = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)
outerGuard = "101"
centerGuard = "01010"

type Pixel = Word8
type RGB = (Pixel, Pixel, Pixel)
type Pixmap = Array (Int, Int) RGB

parseRawPPM :: P.Parse Pixmap
parseRawPPM = P.parseWhileWith P.w2c (/= '\n') P.==> \header ->
  P.skipSpaces
    P.==>& P.assert (header == "P6") "invalid raw header"
    P.==>& P.parseNat
    P.==>  \width -> P.skipSpaces P.==>& P.parseNat P.==> \height ->
             P.skipSpaces P.==>& P.parseNat P.==> \maxValue ->
               P.assert (maxValue == 255) "max value out of spec"
                 P.==>& P.parseByte
                 P.==>& parseTimes (width * height) parseRGB
                 P.==>  \pxs -> P.identity
                          (listArray ((0, 0), (width - 1, height - 1)) pxs)

parseRGB :: P.Parse RGB
parseRGB = P.parseByte P.==> \r ->
  P.parseByte P.==> \g -> P.parseByte P.==> \b -> P.identity (r, g, b)

parseTimes :: Int -> P.Parse a -> P.Parse [a]
parseTimes 0 _ = P.identity []
parseTimes n p = p P.==> \x -> (x :) <$> parseTimes (n - 1) p

-- 아래는 타입을 정의하는 것이므로, RunLength는 data constructor가
-- 튜플로 갖는 타입에 따라 타입 a가 정의된다.
type Run = Int
type RunLength a = [(Run, a)]

runLength :: Eq a => [a] -> RunLength a
runLength = map rle . group where rle xs = (length xs, head xs)

type Score = Ratio Int

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map divide xs
 where
  divide d = fromIntegral d / divisor
  divisor = fromIntegral (sum xs)

type ScoreTable = [[Score]]
type Digit = Word8

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
 where
  scores = zip [ distance d (scaleToOne ps) | d <- srl ] digits
  digits = [0 .. 9]

-- SRL은 Scaled Run Length라는 의미이다.
asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList

-- zipWith로 a와 b를 합치면서 (적은 쪽으로 원소 개수 통합) 전달한
-- 함수를 함께 수행한다.
distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

-- RGB 기반으로 greyscale 이미지로 변환하는 함수를 구현한다.
luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r, g, b) = round (r' * 0.30 + g' * 0.59 + b' * 0.11)
 where
  r' = fromIntegral r
  g' = fromIntegral g
  b' = fromIntegral b

type Greymap = Array (Int, Int) Pixel

pixelToGreymap :: Pixmap -> Greymap
pixelToGreymap = fmap luminance

data Bit = Zero | One
         deriving (Eq, Show)

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
 where
  binary i | i < pivot = Zero
           | otherwise = One
  pivot    = round $ least + (greatest - least) * n
  least    = fromIntegral $ choose (<) a
  greatest = fromIntegral $ choose (>) a
  choose f = foldA1 $ \x y -> if f x y then x else y

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . runLength

data Parity a = Even a | Odd a | None a
              deriving (Show)

-- parity table에서 찾은 것인지 odd table에서 찾은 건지 확인하기 위해
-- 아래와 같이 함수를 구현한다.
fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd  a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd  a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
  fmap = parityMap

-- `on`은 Data.Function 에 있는 함수로서 아래와 같은 타입 정의를 갖는다.
-- on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
-- on f g x y = g x `f` g y
-- 타입을 살펴보면, 함수 2개를 받아 2번째 함수를 1번째 함수에 그대로 적용한다.
compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity ((map Odd (bestScores leftOddSRL ps)) ++
                                          (map Even (bestScores leftEvenSRL ps)))

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

-- 아래와 같이 안하는 이유는 기대하는 결과값이 달라지기 때문
-- length . show $ AltEven 1  --> 27의 결과값을 출력한다.
data AltParity a = AltEven {fromAltParity :: a}
                 | AltOdd  {fromAltParity :: a}
                 | AltNone {fromAltParity :: a}
                 deriving (Show)

chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f xs = let (h, t) = f xs
                 in h : chunkWith f t

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_. One) : _) = []
candidateDigits rle | length rle < 59 = []

type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)

updateMap :: Parity Digit
          -> Digit
          -> [Parity Digit]
          -> ParityMap
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
  where key' = (key + digit) `mod` 10
