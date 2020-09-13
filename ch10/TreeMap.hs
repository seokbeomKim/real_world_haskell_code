-- file: ch10/TreeMap.hs
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Show)

-- 문자열을 입력 받아 그 길이를 이용하여 트리구조로 변환하기 위해서는
-- 아래와 같이 구현할 수 있다.
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

-- 아래와 같이 ghci에서 테스트해볼 수 있다.
-- let tree = Node (Leaf "foo") (Node (Leaf "x") (Leaf "quux"))
-- treeLengths tree
-- treeMap (odd . length) tree

-- 하지만 이러한 treeMap 대신 Functor 타입글래스를 이용하여 구현할 수
-- 있으며, 기본적인 인터페이스를 아래와 같이 구현하면 fmap
-- 인터페이스로 함께 사용할 수 있다.
instance Functor Tree where
  fmap = treeMap
