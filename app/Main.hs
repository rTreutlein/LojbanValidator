{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE Rank2Types         #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables#-}
module Main where

import Lojban

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import System.Process

main :: IO ()
main = do
    validator <- initValidator "cmavo.csv" "gismu.csv"
    mainloop validator

mainloop validator = do
    putStrLn "Please Input some Lojban to Validate"
    input <- getLine

    let res = validator input

    case res of
        Right x -> print x
        Left e  -> putStrLn e

    mainloop validator
