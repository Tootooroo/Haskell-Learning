-- Writer Monad

import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number : " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = logNumber 3 >>= \x -> logNumber (x+5) >>= return

main = do
  print $ logNumber 3
  print $ logNumber 5
  print multWithLog
