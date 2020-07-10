--
-- EPITECH PROJECT, 2020
-- FUN_imageCompressor_2019
-- File description:
-- AlgoK
--

module AlgoK
(
     startAlgo,
     createTabEucli,
     parser
) where

import System.Random
import Data.Char
import System.Exit
import Functions
import TabEucli
import Print
import Control.Monad
import Control.Category
import System.Environment
import System.Exit
import Data.List
import System.IO
import Control.Exception
import Text.Read

noEmptyTab ::
     Struct -> Int -> Int
noEmptyTab struct i
     | i == (length (order struct)) = 0
     | ((length (order struct !! i) == 0)) = 1
     | i /= (length (order struct)) && ((length (order struct !! i) /= 0)) = noEmptyTab struct (i + 1)
     | otherwise = 1

checkPrintResult ::
     [String] -> [[Int]] -> [Int] -> [[Int]] -> Struct -> IO ()
checkPrintResult args list rand tab struct
    | (noEmptyTab struct 0) == 1 = parser args
    | otherwise = printResult struct 0

loopDiffOldNew ::
     [String] -> Struct -> Int -> Int
loopDiffOldNew args struct i
     | i == (length (tabNewRandEucli struct)) = 0
     | (abs(tabOldRandEucli struct !! i - tabNewRandEucli struct !! i)) < (read :: String -> Float) (args !! 1) = loopDiffOldNew args struct (i+1)
     | otherwise = 1

modifMatrixAverage ::
     Int -> Float -> Float -> Float -> [[Float]] -> [[Float]]
modifMatrixAverage i r g b tab = do
     let tabR = updateMatrix tab r (i, 0)
     let tabG = updateMatrix tabR g (i, 1)
     let tabB = updateMatrix tabG b (i, 2)
     return tabB !! 0

calculAverage ::
     Struct -> [[Float]] -> Int -> Int -> Float -> Float -> Float -> [[Float]]
calculAverage struct tab i j r g b
     | j /= (length (order struct !! i)) = calculAverage struct tab i (j + 1)
     (r + (toFloat (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 0)))
     (g + (toFloat (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 1)))
     (b + (toFloat (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 2))) 
     | otherwise = modifMatrixAverage i (r / (toFloat(length (order struct !! i)))) (g / (toFloat(length (order struct !! i)))) (b / (toFloat(length (order struct !! i)))) tab

upAverageOrder ::
     Struct -> Int -> [[Float]] -> [[Float]]
upAverageOrder struct nb tab
     | (nb + 1) /= (length (order struct)) && (length (order struct !! (nb + 1))) /= 0  = upAverageOrder struct (nb + 1) (calculAverage struct tab (nb + 1) 0 0 0 0)
     | (nb + 1) /= (length (order struct)) = upAverageOrder struct (nb + 1) tab
     | otherwise = return tab !! 0


setupUpTabDiff ::
     Struct -> [Int] -> Struct
setupUpTabDiff struct rand
     | abs(((tabEucli struct !! (i struct)) - (tabRandEucli struct !! (j struct)))) <= (diff struct) =
     struct {diff=(abs(((tabEucli struct !! (i struct)) - (tabRandEucli struct !! (j struct))))), backup=(j struct)}
     | otherwise = return struct !! 0

appendOrder ::
     Struct -> [[Int]] -> [[Int]]
appendOrder struct tab = do
     let (beginning, end) = splitAt (backup struct) tab
     let desired = head end
     let desired' = (i struct):desired
     let end' = desired' : (tail end)
     beginning ++ end'

updateTabDiff ::
     Struct -> [Int] -> Struct
updateTabDiff struct rand
     | ((j struct) + 1) /= (length rand) =
     updateTabDiff (setupUpTabDiff struct {j=((j struct)+1)} rand) rand
     | otherwise = struct {order=(appendOrder struct (order struct))}

checkTabDiff ::
     [Int] -> Struct -> Struct
checkTabDiff rand struct
     | ((i struct) + 1) /= (length (tabEucli struct)) =
     checkTabDiff rand (updateTabDiff (struct {i=((i struct)+1), j=(-1), diff=(abs(((tabEucli struct !! ((i struct)+1)) - (tabRandEucli struct !! 0))))}) rand)
     | otherwise = return struct !! 0

fillOrder ::
     [[Int]] -> Int -> Int -> [[Int]]
fillOrder order i nb
     | i /= nb = fillOrder (order ++ [[]]) (i + 1) nb
     | otherwise = return order !! 0

createTabEucli ::
     [String] -> [[Int]] -> [Int] -> [[Int]] -> Struct -> IO ()
createTabEucli args list rand tab struct = do
     let orderS = checkTabDiff rand struct {pixelList=list, order=(fillOrder [] 0 (length rand)), tabEucli=(tabDistEucli list [] 1),
     tabRandEucli=(tabRandDistEucli list struct [] 0), diff=0, i=(-1), backup=0, j=(-1)}
     let finalStruct = orderS {randArr=(upAverageOrder orderS (-1) (randArr orderS)), j=(length (order finalStruct)), tabOldRandEucli=(fillOldTabEucli orderS)}
     let structs = finalStruct {tabNewRandEucli=(tabRandDistEucli list finalStruct [] 0)}
     if (loopDiffOldNew args structs 0) == 0 then checkPrintResult args list rand tab structs else createTabEucli args list rand tab structs

getRandom ::
     [String] -> [[Int]] -> Int -> [Int] -> Struct -> IO ()
getRandom args list nb lRand struct = do
     randomList <- sequence $ replicate 1 $ randomRIO (0,(length (tabRand struct)-1))
     let rand = lRand ++ [(tabRand struct !! (randomList !! 0))]
     if ((read :: String -> Int) (args !! 0)) /= nb
     then getRandom args list (nb + 1) rand (struct {tabRand=(deleteAt (randomList !! 0) (tabRand struct)), randArr=((randArr struct ++ [toListFloat (list !! (tabRand struct !! (randomList !! 0)))]))})
     else createTabEucli args list rand [[]] (struct {randArr=((randArr struct ++ [toListFloat (list !! (tabRand struct !! (randomList !! 0)))]))})

startAlgo ::
     [FilePath] -> [[Int]] -> Struct-> IO ()
startAlgo args list struct
    | (read :: String -> Float ) (args !! 1) <= 0 = error "Error the second argument must be > 0"
    | (read :: String -> Int) (args !! 0) * 2 > (length list) = error "Error in list"
    | otherwise = getRandom args list 1 [] struct

-------------------------- PARSER ------------------------------------

exitWithErrorMessage :: 
     String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

exitFile ::
     IO a
exitFile = exitWithErrorMessage "" (ExitFailure 84)

stringToInt ::
     String -> Int
stringToInt input =
  case readMaybe input of
      Nothing -> error "error in list"
      Just age -> case age of { _ -> age }

splitEvery2 ::
     Int -> [a] -> [[a]]
splitEvery2 _ [] = []
splitEvery2 n xs = as : splitEvery (n - 1) bs
  where (as,bs) = splitAt n xs

splitEvery ::
     Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery2 (n + 1) bs
  where (as,bs) = splitAt n xs

fillListRand ::
     [[Int]] -> [Int]
fillListRand list = filter odd [0..(length list)]

parser ::
     [FilePath] -> IO ()
parser args = do
    let filePath = args !! 2
    text <- readFile filePath
    let repl ',' = ' '
        repl c = c
    let space = map (repl) text
    let word = words space
    let par1 = map (\\ "(") word
    let par2 = map (\\ ")") par1
    let fusion = map stringToInt par2
    let list = splitEvery 2 fusion
    let struct = Struct {order=[], tabEucli=[], tabRandEucli=[], tabOldRandEucli=[],
    tabNewRandEucli=[], randArr=[], pixelList=[], tabRand=(fillListRand list), diff=0, i=0, backup=0, j=0}
    startAlgo args list struct