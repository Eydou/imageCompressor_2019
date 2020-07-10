--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- functions
--

module Functions
(
    joinInt,
    replaceAtIndex,
    deleteAt,
    toPair,
    toFloat,
    toInt,
    toListFloat,
    updateMatrix,
    Struct(..)
) where

import Data.Char
import System.Exit

data Struct = Struct {
    order :: [[Int]],
    randArr :: [[Float]],
    pixelList :: [[Int]],
    tabEucli :: [Float],
    tabRandEucli :: [Float],
    tabOldRandEucli :: [Float],
    tabNewRandEucli :: [Float],
    tabRand :: [Int],
    diff :: Float,
    i :: Int,
    j :: Int,
    backup :: Int
} deriving (Show)

joinInt ::
    [Int] -> Int
joinInt l = read $ map intToDigit l

replaceAtIndex ::
    Int -> a -> [a] -> [a]
replaceAtIndex n item ls =
    a ++ (item:b) where (a, (_:b)) = splitAt n ls

deleteAt ::
    Int -> [a] -> [a]
deleteAt idx items = take idx items ++ drop (1 + idx) items

toInt ::
    Float -> Int
toInt x
    | (x - (toFloat (floor x))) == 0.5 = floor x
    | otherwise = round x

toFloat ::
    Int -> Float
toFloat x = (fromIntegral x)

toListFloat ::
    [Int] -> [Float]
toListFloat xs = map (\x -> fromIntegral x) xs

toPair ::
    [Int] -> [Int]
toPair list =
    replaceAtIndex 0 ((joinInt list)* 2 + 1) list

updateMatrix ::
    [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) =
    take r m ++
    [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
    drop (r + 1) m