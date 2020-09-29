-- Carrier.hs: Maybe를 모나드로 이용하는 예제이다.
import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type MobileCarrier = String
type BillingAddress = String

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress

-- findCarrierBillingAddress person phoneMap carrierMap addressMap =
firstVariation person phoneMap carrierMap addressMap =
  -- 모나드가 없이 구현한 버전은 이와 같이 반복적으로 계속해서
  -- 호출하도록 구현해야 한다. 가독성도 떨어질 뿐 아니라 전체적인
  -- 코드가 깔끔하지 못하다.
  case M.lookup person phoneMap of
    Nothing -> Nothing
    Just number ->
      case M.lookup number carrierMap of
        Nothing -> Nothing
        Just carrier -> M.lookup carrier addressMap

secondVariation person phoneMap carrierMap addressMap = do
  -- 실제로 return을 수행할 필요는 없다.
  number <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address

findCarrierBillingAddress person phoneMap carrierMap addressMap =
    lookup phoneMap person >>=
    lookup carrierMap >>=
    lookup addressMap
  where lookup = flip M.lookup
