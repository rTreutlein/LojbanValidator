{-# LANGUAGE BangPatterns #-}
module Lojban
( loadWordLists
, WordList
, wGismus
, wBai
, ADT
, initValidator
, validateLojban
, cmevla
, brivla
, morph
, sepMorph
, optMorph
) where

import Lojban.WordList
import Lojban.Syntax
import Lojban.Syntax.Morph
import Lojban.Syntax.Types
import Lojban.Syntax.Util

import Control.Monad.IO.Class
import Control.Monad.RWS
--import Control.Monad.Trans.Class

import Control.Exception
import System.Random
import Data.Char (chr)
import Data.Maybe
import qualified Data.Map as M


import Iso hiding (Syntax,SynIso)

initValidator :: IO (String -> Either String ADT)
initValidator = do
    wordlist <- loadWordLists
    return (validateLojban wordlist)

validateLojban :: (WordList State) -> String -> Either String ADT
validateLojban rstate text = let !res = head . fst <$> evalRWST (apply lojban ()) rstate state
                             in res
    where state = State {sText = text++" "}
