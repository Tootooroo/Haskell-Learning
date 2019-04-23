module Modules.Tools
(doubleMe,
 firstLetter,
 secondLetter,
 wholeLetter,
 isValid,
 caseTry,
 rightTriangles,
 addVectors,
 factorial,
 quicksort,
 op
) where

doubleMe :: Int -> Int
doubleMe a = a + a

firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter all@(x:xs) = [x]

secondLetter :: String -> String
secondLetter "" = "Empty String"
secondLetter all@(x:y:ys) = [y]

wholeLetter :: String -> String
wholeLetter "" = "Empty String"
wholeLetter all@(x:y:ys) = all

isValid :: Int -> Int -> String
isValid wet temp
    | check wet temp == True = "Invalid"
    | check wet temp == False = "Valid"
    where check a b = a * b < 0 

caseTry :: Int -> String
caseTry x = case x of
                0 -> "Zero"
                1 -> "One"
                2 -> "Two"

rightTriangles = [ (a, b, c) | a <- [1..10], b <- [1..10], 
    c <- [1..10], a^2 + b^2 == c^2 ]

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

factorial :: Int -> Int
factorial x 
    | x == 1 = 1
    | otherwise = x * factorial (x-1)

quicksort :: (Ord a) => [a] -> [a]
quicksort (x:xs) =
    let smallerOrEqual = [ a | a <- xs, a <= x]
        larger = [ a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

op :: (Int -> Int -> Int) -> Int -> Int -> Int
op f x y = (f x) y


