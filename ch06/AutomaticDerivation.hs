data CannotShow = CannotShow deriving (Show)

data OK = OK

instance Show OK where
  show _ = "OK"