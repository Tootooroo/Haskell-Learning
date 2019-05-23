-- try.hs

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Modules.Tools as Tools
import qualified Data.Map as Map
import qualified Modules.DataStructs as DataS
import Control.Parallel.Strategies
import Control.DeepSeq
import Control.Exception

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
area :: Shape -> Float
area (Circle _ r) = pi * r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

type Index = Int
type Value = String
type PhoneBook k v = [(k, v)]

phoneBook :: Map.Map Index Value
phoneBook = Map.fromList $ [ (1, "One"), (2, "Two"), (3, "Three") ]
findKey :: Index -> [(Index, Value)] -> Maybe Value
findKey key [] = Nothing
findKey key ((k, v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

type ValMap = Map.Map Index Value
valLookup :: Int -> ValMap -> Either String String
valLookup idx map = case Map.lookup idx map of
    Nothing -> Left $ "Index" ++ show idx ++ "not exist"
    Just val -> Right $ val


-- Class
class (Eq a) => Equal a where
    isEqual :: a -> a -> Bool

data Number = Number Int deriving (Eq)
instance Equal Number where
    isEqual x y = if x == y then True else False
instance (Equal m) => Equal (Maybe m) where
    isEqual x y = if x == y then True else False

data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True

data Box a = Box a | None deriving (Show)
instance Functor Box where
    fmap f (Box a) = Box (f a)
    fmap f None = None

solveRPN :: String -> Double
solveRPN = head . foldl foldFunc [] . words
    where foldFunc (x:y:ys) "*" = (x * y):ys
          foldFunc (x:y:ys) "+" = (x + y):ys
          foldFunc (x:y:ys) "-" = (x - y):ys
          foldFunc (x:y:ys) "/" = (x / y):ys
          foldFunc xs numberString = read numberString:xs

data Section = Section { roadA :: Int, roadB :: Int, roadC :: Int }
type RoadSystem = [Section]
data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let totalA = sum $ map snd pathA
        totalB = sum $ map snd pathB
        toAFromA = totalA + a
        toAFromB = totalB + b + c
        toBFromA = totalA + a + c
        toBFromB = totalB + b

        newPathToA = if toAFromA <= toAFromB
                     then (A, a):pathA
                     else (C, c):(B, b):pathB
        newPathToB = if toBFromA <= toBFromB
                     then (C, c):(A, a):pathA
                     else (B, b):pathB
    in (newPathToA, newPathToB)

bestPath :: RoadSystem -> Path
bestPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if (sum (map snd bestAPath)) <= (sum (map snd bestBPath))
       then reverse bestAPath
       else reverse bestBPath
headThrowLondon :: RoadSystem
headThrowLondon = [ Section 50 10 30,
                    Section 5 90 20,
                    Section 40 2 25,
                    Section 10 8 0
                  ]

data MyNum a = MyNum a deriving (Show)
instance Functor MyNum where
   fmap f (MyNum n) =  MyNum (f n)

instance Applicative MyNum where
    pure = MyNum
    (MyNum f) <*> (MyNum a) = fmap f (MyNum a)

data Things a = Things a deriving (Show)
instance Functor Things where
    fmap f (Things a) = Things (f a)

data HereOr a b = Here a | There b
instance Functor (HereOr a) where
    fmap f (Here a) = Here a
    fmap f (There b) = There (f b)

foldll :: (b -> a -> b) -> b -> [a] -> b
foldll f b [] = b
foldll f b (x:xs) = let r = f b x
                    in foldll f r xs

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> x -> [x]
replicate' n _
    | n <= 0 = []
replicate' n x = x : replicate' (n-1) x

take' :: Int -> [xs] -> [xs]
take' n _
    | n <= 0 = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse_fold :: [a] -> [a]
reverse_fold = foldl (\acc x -> x : acc) []

repeat' :: a -> [a]
repeat' x = x : repeat' x

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

zip' :: [l] -> [r] -> [(l, r)]
zip' [] _ = []
zip' _ [] = []
zip' (l:ls) (r:rs) = (l, r) : zip' ls rs

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

applyTwice :: (a -> a) -> a -> a
applyTwice f a = f (f a)

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

doSomething :: Float
doSomething =
  let point = Point 1 1
      c = Circle point 1
  in area c

test_main = do
    print $ (Number 1) == (Number 1)
    print $ doSomething
    print $ sum' [1,2,3]
    print $ zipWith' (+) [1,2,3] [1,2,3]
    print $ applyTwice divideByTen 200
    print $ divideByTen 200
    print $ elem' 1 [2,1,3]
    print $ elem' 1 [2,3,4]
    print $ zip' [1,2,3] [4,5,6]
    print $ take' 4 (repeat' 5)
    print $ reverse' [1,2,3,4,5]
    print $ reverse_fold [1,2,3,4,5]
    print $ take' 4 [1,2,3,4,5]
    print $ maximum' [1,2,3,4]
    print $ replicate' 4 'a'
    print $ bestPath headThrowLondon
    print $ fmap (\x -> x + 1) (MyNum 1)
    print $ (MyNum (\x -> x + 1)) <*> MyNum 1
    print $ foldll (\x y -> x + y) 0 [1, 2]

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing
x -: f = f x

landBirds_main = do
  print $ pure (0, 0) >>= landLeft 1 >>= landRight 5

monad_main = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

parallel_main = runEval $ do
  a <- rpar (force (map (+5) [1..5000000]) :: [Int])
  b <- rpar (force (map (+5) [5000001..10000000]) :: [Int])
  return (a,b)

main = catch (print $ div 5 0) handler
  where handler :: SomeException -> IO ()
        handler ex = putStrLn $ "Caught exception: " ++ show ex
