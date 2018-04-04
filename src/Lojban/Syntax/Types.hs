{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
module Lojban.Syntax.Types where

import Prelude hiding (id,(.),(<*>),(<$>),pure,(*>),(<*),foldl)


import Data.List (partition,isPrefixOf,isSuffixOf,nub,any,intercalate)
import Data.Maybe (fromJust)
import qualified Data.Map as M

import Iso
import Syntax (SyntaxState)
import qualified Syntax

import Control.Monad.RWS
import Control.DeepSeq
import GHC.Generics (Generic,Generic1)

import qualified Data.ListTrie.Patricia.Set.Ord as TS

type StringSet = TS.TrieSet Char

--They Iso we are using
--We use a RWST monad over (Either String) for failurs
--currently the writer part is unused
type SynIso s a b = Syntax.SynIso (RWST (WordList s) () s) a b
type Syntax s a   = SynIso s () a

--The Reader Contents
--gismus : A wordlist of all gisms
--bai    : A mapping from bai to their corresponding gismu
data WordList s = WordList { wGismus :: StringSet
                           , wBai    :: SynIso s String String
                           }
--The State
--sText  : The actuall text to be parsed
data State = State { sText :: String
                   } deriving Show

--The parser Requirse that the State is a SyntaxState
instance SyntaxState State where
    getText = sText
    addText str sta = sta {sText = str ++ (sText sta)}
    setText str sta = sta {sText = str}

data ADT = Leaf String | Node String [ADT]
         deriving (Show,Generic,NFData)
