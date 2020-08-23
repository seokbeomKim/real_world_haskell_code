data Color = Red | Green | Blue

class BasicEq3

instance BasicEq3 Color where
  isEqual3 Red Red = True
  isEqual3 Blue Blue = True
  isEqual3 Green Green = True
  isEqual3 _ _ = False