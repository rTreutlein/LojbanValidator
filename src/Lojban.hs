module Lojban
( loadWordLists
, WordList
, cmavos
, gismus
, bai
, ADT
, initValidator
, validateLojban
) where

import Lojban.WordList
import Lojban.Syntax
import Lojban.Syntax.Types

import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Exception
import System.Random
import Data.Char (chr)
import Data.Maybe
import qualified Data.Map as M

import Control.Monad.RWS
import Control.Monad.Trans.Class

import Iso hiding (Syntax,SynIso)

initValidator :: String -> String -> IO (String -> Either String ADT)
initValidator cmavoSrc gismuSrc = do
    wordlist <- loadWordLists cmavoSrc gismuSrc
    return (validateLojban wordlist)

validateLojban :: (WordList State) -> String -> Either String ADT
validateLojban rstate text = head . fst <$> evalRWST (apply lojban ()) rstate state
    where state = State {sText = text++" "}
