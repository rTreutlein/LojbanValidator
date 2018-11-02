{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Prelude hiding (id,(.),(<*>),(<$>),pure,(*>),(<*),foldl,print)
import qualified Prelude as P

import Lojban
import Lojban.WordList
import Lojban.Syntax
import Lojban.Syntax.Util
import Lojban.Syntax.Types
import Lojban.Syntax.Morph

import Iso hiding (Syntax,SynIso)
import Syntax hiding (Syntax,SynIso)

import Control.Applicative ((<$>))
import Control.Monad.RWS
import Control.Category (id,(.))

import qualified Data.ListTrie.Patricia.Set.Ord as TS

import Text.Parsers.Frisby (PM,P,runPeg)

import Data.Maybe
import qualified Data.Map as M

--import Text.XML.HXT.Core

mystate s = State {sText = s}

loadwl = do
    (wl :: WordList State) <- loadWordLists
    return wl

mpag :: WordList State -> Syntax State a -> String -> Either String (a,State,())
mpag wl x y = runRWST (apply x ()) wl (mystate y)
