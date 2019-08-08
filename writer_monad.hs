import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Cont
import Control.Monad.Reader
import List.Transformer
import qualified System.IO

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number : " ++ show x])

type Stack = [Int]

pop :: Int -> State Stack Int
pop _ = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

plusOne :: Int -> State Int Int
plusOne x = state $ \x -> (x, x + 1)

plusAction :: State Int Int
plusAction = do
  x <- get
  put $ x + 1
  y <- get
  put $ y + 1
  return y

test :: String -> ListT IO String
test str = do
  s <- liftIO getLine
  return (str++s)

strings :: ListT IO String
strings = do
    _ <- select (repeat ())
    liftIO (putStrLn "Say something:")
    liftIO getLine

stdin :: ListT IO String
stdin = ListT (do
    eof <- getLine
    if eof == "end"
        then return Nil
        else do
            string <- getLine
            return (Cons string stdin) )

retrive :: ListT IO String -> IO String
retrive l = fold (++) "" reverse l

main = do
  x <- retrive stdin
  print $ x
