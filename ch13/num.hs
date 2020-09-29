-- ch13/num.hs
import Data.List

-- 하스켈에서의 타입 시스템을 이용하여, 기본적인 사칙연산에 통합하는
-- 방법에 대한 예제를 보여준다.
data Op = Plus | Minus | Mul | Div | Pow
        deriving (Eq, Show)

data SymbolicManip a =
  Number a |
  Symbol String |
  BinaryArith Op (SymbolicManip a) (SymbolicManip a) |
  UnaryArith String (SymbolicManip a)
  deriving (Eq)

instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = BinaryArith Mul (Number (-1)) a
  abs a = UnaryArith "abs" a
  signum _ = error "signum is unimplemented"
  fromInteger i = Number (fromInteger i)

instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a
  asin a = UnaryArith "asin" a
  acos a = UnaryArith "acos" a
  atan a = UnaryArith "atan" a
  sinh a = UnaryArith "sinh" a
  cosh a = UnaryArith "cosh" a
  tanh a = UnaryArith "tanh" a
  asinh a = UnaryArith "asinh" a
  acosh a = UnaryArith "acosh" a
  atanh a = UnaryArith "atanh" a

-- 위에서 정의한 타입을 출력하기 위한 formatting 함수를 정의한다.
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x

prettyShow (BinaryArith op a b) =
  let pa = simpleParen a
      pb = simpleParen b
      pop = op2str op
      in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a) => SymbolicManip a -> String
simpleParen (Number a) = prettyShow (Number a)
simpleParen (Symbol a) = prettyShow (Symbol a)
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
  show a = prettyShow a

rpnShow :: (Show a, Num a) => SymbolicManip a -> String
rpnShow i =
  let toList (Number x) = [show x]
      toList (Symbol x) = [x]
      toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
      toList (UnaryArith op a) = toList a ++ [op]
      join :: [a] -> [[a]] -> [a]
      join delim l = concat (intersperse delim l)
  in join " " (toList i)

simplify :: (Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
  let sa = simplify ia
      sb = simplify ib
      in
      case (op, sa, sb) of
          (Mul, Number 1, b) -> b
          (Mul, a, Number 1) -> a
          (Mul, Number 0, b) -> Number 0
          (Mul, a, Number 0) -> Number 0
          (Div, a, Number 1) -> a
          (Plus, a, Number 0) -> a
          (Plus, Number 0, b) -> b
          (Minus, a, Number 0) -> a
          _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x
