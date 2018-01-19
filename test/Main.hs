{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE PartialTypeSignatures#-}

module Main where

import Lojban

import Control.Exception
import Control.Monad

import Control.Parallel.Strategies

import System.Exit (exitFailure,exitSuccess)

main :: IO ()
main = do
    putStrLn "Starting Validation"
    validator <- initValidator "cmavo.csv" "gismu.csv"
    sentences <- loadData
    let x = map (validate validator) sentences
    validationres <- sequence x
    let testResF  = filter id validationres
    putStrLn $
        "Of " ++ show (length sentences) ++ " sentences " ++
        show (length testResF) ++ " have been validated successfully."
    if length testResF == length sentences
        then exitSuccess
        else exitFailure

mycount :: Either String ADT -> Int -> Int
mycount ei i = case ei of
    Left  _ -> i
    Right _ -> i+1

validate' :: Either String ADT -> IO Bool
validate' res = do
    case res of
        Left e  -> putStrLn e >> putStrLn "" >> return False
        Right _ -> return True

validate :: (String -> Either String ADT) -> String -> IO Bool
validate validator text = do
    print text
    case validator text of
        Left e  -> putStrLn e >> putStrLn "" >> return False
        Right _ -> return True

loadData :: IO [String]
loadData = do
    file <- readFile "data.txt"
    return (lines file)

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
