-- Parse.hs 예제 작성하며 실제 Functor 구현하여 사용해보기

data TestData = DataConstructor {
  dString :: String,
  dInt :: Int
} deriving (Show, Eq)

x = DataConstructor "my own string" 1
y = DataConstructor {
  dString = "my own string",
  dInt = 20
}

newtype Parse a = Parse {
  runParse :: TestData -> Either String (a, TestData)
}

-- 예제에서는 runParse를 명시적으로 기술하는 대신에, identity a =
-- Parse (\s -> Right (a, s)) 와 같이 표기하였다.
runParseFunc :: a -> Parse a
runParseFunc a = Parse {
  runParse = \s -> Right (a, s)
}
