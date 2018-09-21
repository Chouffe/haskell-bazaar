module Environment
  (Environment (Prod, Dev, Test))
  where

data Environment
  = Prod
  | Dev
  | Test
  deriving (Eq, Show)
