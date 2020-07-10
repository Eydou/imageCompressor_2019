--
-- EPITECH PROJECT, 2020
-- source
-- File description:
-- Main
--

module Main where


import System.Environment
import System.IO
import Check

main :: IO ()
main = getArgs >>= checkErrors