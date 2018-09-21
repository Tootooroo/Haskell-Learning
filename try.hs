{-# LANGUAGE DeriveAnyClass #-}

import qualified Modules.Tools as Tools
import qualified Data.Map as Map
import qualified Modules.DataStructs as DataS

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
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
    isEqual :: a -> Bool

data Number = Number Int deriving (Eq)
instance Equal Number where
    isEqual x = if x == (Number 1) then True else False


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


main = do 
    print $ isEqual (Number 2) 

