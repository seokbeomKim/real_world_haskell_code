import Control.Monad

type Phone = String
data Context = Home | Mobile | Business
             deriving (Eq, Show)

albulena = [(Home, "+355-652-55512")]
nils = [(Mobile, "+47-923-64-1234"),
        (Business, "+47-123-45-6789"),
        (Home, "+47-000-00-0001"),
        (Business, "+47-222-22-2222")]

twalumba = [(Business, "+260-02-55-5121")]

{- | Version 1: case .. of 구문 사용 -}
-- onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
-- onePersonalPhone ps = case lookup Home ps of
--                         Nothing -> lookup Mobile ps
--                         Just n -> Just n

-- 한 명이 여러 개의 Business Phone을 가지고 있는 경우를 가려내기 위한
-- 함수 구현: 한 명이 가지고 있는 여러 개의 번호들을 리ㅅ트 형태로
-- 반환한다.
-- allBusinessPhones :: [(Context, Phone)] -> [Phone]
-- allBusinessPhones ps = map snd numbers
--   where numbers = case filter (contextIs Business) ps of
--                     [] -> filter (contextIs Mobile) ps
--                     ns -> ns
--         contextIs a (b, _) = a == b

{-| Version 2: MonadPlus 사용-}

-- case .. of 구문으로 계단식으로 묶어야 했던 것을 MonadPlus를
-- 이용하여 아래와 같이 나타낼 수 있다.
oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                 filter (contextIs Mobile) ps
  where contextIs a (b, _) = a == b
