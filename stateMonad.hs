-- State Monad

import Control.Monad.Trans.State.Lazy
import System.Random

type RandomState a = State StdGen a

getRandom_here :: Random a => RandomState a
getRandom_here =
  get >>= \gen -> let (val, gen') = random gen in
                    put gen' >> return val

main = do
  gen <- getStdGen
  let (result, newState) = runState getRandom_here gen
  print (result :: Int)

