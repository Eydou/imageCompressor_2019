--
-- EPITECH PROJECT, 2020
-- source
-- File description:
-- Check
--

module Check
(   exitWithErrorMessage,
    exitHelp,
    isNum,
    isFloat,
    checkErrors,
) where

import System.Exit
import System.IO
import Data.Char
import AlgoK
import Control.Exception

exitWithErrorMessage ::
    String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

exitHelp ::
    IO a
exitHelp = exitWithErrorMessage "USAGE: ./imageCompressor n e IN\nn number of colors in the final image\ne convergence limit\nIN path to the file containing the colors of the pixels" (ExitFailure 84)

isNum ::
    [Char] -> Bool
isNum [] = True
isNum (x:xs)
    | x == '-' = False
    | isDigit x == False = False
    | otherwise = isNum(xs)

isFloat ::
    String -> Bool
isFloat ""  = False
isFloat "." = False
isFloat xs  =
    case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

startParser ::
    [FilePath] -> IO b
startParser args = do
    isErr <- try (parser args) :: IO (Either SomeException ())
    case isErr of
        Left err -> do
            print err
            exitWith $ ExitFailure 84
        Right _ -> exitSuccess

checkErrors ::
    [[Char]] -> IO ()
checkErrors args
    | length args /= 3 = exitHelp
    | isNum (args !! 0) == False = exitHelp
    | isFloat (args !! 1) == False = exitHelp
    | otherwise = startParser args