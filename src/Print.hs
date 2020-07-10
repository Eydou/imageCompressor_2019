module Print
(
    printResult
) where

import Functions
import Text.Printf

loopPrintOrder ::
    Struct -> Int -> Int -> IO ()
loopPrintOrder struct i j = do
    printf("(%i,%i) (%i,%i,%i)\n") (pixelList struct !! ((order struct !! i !! j) * 2) !! 0) (pixelList struct !! ((order struct !! i !! j) * 2) !! 1) (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 0) (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 1) (pixelList struct !! ((order struct !! i !! j) * 2 + 1) !! 2)
    if ((j + 1) /= length (order struct !! i)) then loopPrintOrder struct i (j + 1) else printf("")

printResult ::
    Struct -> Int -> IO ()
printResult struct i = do
    printf("--\n")
    printf("(%i,%i,%i)\n") (toInt(randArr struct !! i !! 0)) (toInt(randArr struct !! i !! 1)) (toInt(randArr struct !! i !! 2))
    printf("-\n")
    loopPrintOrder struct i 0
    if ((i + 1) /= (length (randArr struct))) then printResult struct (i + 1) else printf("")