-- ch13/buildmap.hs

-- 책의 내용에 따르면, "잘 구현된 Map은 해시 테이블과 성능이
-- 비슷하다"고 기술되어 있다. Association List의 경우 간단한 리스트
-- 형태로서 하스켈에서 기본적으로 사용되는 리스트이다. Imperative
-- Language에서 다루는 배열과는 다른 개념이고, 오히려 파이썬에서의
-- List와 비슷하다.
--
-- 단순한 데이터 집합일 경우에는 AL로도 충분하지만 데이터가 많아질
-- 경우에는 Map으로 재구성하는 편이 성능면으로도 유리하다.
--
-- Maps give us the same capabilities as hash tables do in other
-- languages. Internally, a map is implemented as a balanced binary
-- tree.

import qualified Data.Map as Map

-- Functions to generate a Map that represents an association list as
-- a map

al = [(1, "one"), (2, "two"), (3, "three"), (4, "four")]

-- Create a map representation of 'al' by converting the association
-- list using Map.fromList
mapFromAL = Map.fromList al

{- | Create a map representation of 'al' by doing a fold -}
mapFold =
  foldl (\map (k, v) -> Map.insert k v map) Map.empty al

{- | Manually create a map with the elements of 'al' in it -}
mapManual =
  Map.insert 2 "two" .
  Map.insert 4 "four" .
  Map.insert 1 "one" .
  Map.insert 3 "three" $ Map.empty

-- 중요한 것은, Map에 직접 데이터를 추가했을 때 ordering이 보잠되지
-- 않는 다는 점이다.
