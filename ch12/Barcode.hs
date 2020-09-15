-- EAN-13 바코드 해석 코드

import Data.Array (Array(..), (!), bounds, elems, indices, ixmap, listArray)
import Control.Applicative ((<$>))
import Data.Char (digitToInt)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = (sum products `mod` 10)
  where products = mapEveryOther (*3) (reverse ds)

mapEveryOther :: (a -> a) -> [a] -> [a]
mapEveryOther f = zipWith ($) (cycle [f,id])

leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

-- leftOddList는 [ [Char] ] 이므로, [Char] 에 대해 complement를 fmap
-- 해준 후에 이를 다시 map으로 해줘야 한다.
rightList = map complement <$> leftOddList
  where complement '0' = '1'
        complement '1' = '0'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0,l-1) xs
  where l = length xs

leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList
