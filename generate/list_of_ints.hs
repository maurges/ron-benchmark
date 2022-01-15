#!/usr/bin/env stack
-- stack --resolver=lts-18.10 runhaskell --package bytestring

{-# LANGUAGE LambdaCase, BangPatterns #-}

import Data.ByteString.Builder (hPutBuilder, char7, string7, intDec)
import System.Environment (getArgs)
import System.IO (withFile, IOMode (WriteMode))

main = do
    name <- getArgs >>= \case
        [] -> pure "input/list_of_ints.ron"
        "--help":_ -> fail "First arg is filename"
        name:_ -> pure name
    withFile name WriteMode $ flip hPutBuilder buildOutput

buildOutput = char7 '['
           <> mconcat [intDec (rand n) <> string7 ", " | n <- [0..size]]
           <> char7 ']'
    where
        size = 1000000
        rand n = (1664525 * n + 1013904223) `mod` size
