--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- TabEucli
--

module TabEucli
(
    distEucli,
    tabDistEucli,
    tabRandDistEucli,
    fillOldTabEucli,
) where

import Functions

distEucli ::
     [Int] -> [Float] -> [Float] -> [Float]
distEucli firstTab tab secondTab =
     tab ++ [sqrt (((toFloat (firstTab !! 0) - (secondTab !! 0)) ** 2)
     + ((toFloat (firstTab !! 1) - (secondTab !! 1)) ** 2)
     + ((toFloat (firstTab !! 2) - (secondTab !! 2)) ** 2))]

tabDistEucli ::
     [[Int]] -> [Float] -> Int -> [Float]
tabDistEucli list tab i
     | (i - 1) /= length list = tabDistEucli list (distEucli (list !! 1) tab (toListFloat (list !! i))) (i + 2)
     | otherwise = return tab !! 0

tabRandDistEucli ::
     [[Int]] -> Struct -> [Float] -> Int -> [Float]
tabRandDistEucli list struct tab i
     | i /= length (randArr struct) = tabRandDistEucli list struct (distEucli (list !! 1) tab (randArr struct !! i)) (i + 1)
     | otherwise = return tab !! 0

fillOldTabEucli ::
     Struct -> [Float]
fillOldTabEucli struct
     | (length (tabOldRandEucli struct)) == 0 = (tabRandEucli struct)
     | otherwise = (tabNewRandEucli struct)