{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts           #-}
module Lojban.Syntax.Util where

import Prelude hiding (id,(.),(<*>),(<$>),(*>),(<*),foldl)

import Data.List.Split (splitOn)
import Data.List (nub,partition,intercalate,find,delete)
import Data.Char (chr,isLetter,isDigit)
import Data.Maybe
import Data.Map (findWithDefault)

import Control.Category
import Control.Arrow
import Control.Applicative hiding (many,some,optional)
import Control.Monad
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class

import System.Random

import Iso
import Syntax hiding (SynIso,Syntax)
import qualified Syntax

import Lojban.Syntax.Types

import qualified Data.ListTrie.Patricia.Set.Ord as TS

infixr 8 &+&
infixr 5 <&>

(&+&) :: (SyntaxState s,Show a) => Syntax s [a] -> Syntax s [a] -> Syntax s [a]
(&+&) iso1 iso2 = isoConcat . tolist2 . (iso1 &&& iso2)

(<&>) :: SyntaxState s => Syntax s [ADT] -> Syntax s [ADT] -> Syntax s [ADT]
iso1 <&> iso2 = iso1 <+> iso2 <+> (iso1 &+& iso2)

concatMany :: SyntaxState s => Syntax s [a] -> Syntax s [a]
concatMany x = isoConcat . many x

concatSome :: SyntaxState s => Syntax s [a] -> Syntax s [a]
concatSome x = isoConcat . some x

listoptional :: SyntaxState s => Syntax s [a] -> Syntax s [a]
listoptional syn = maybeToList <<< optional syn
    where maybeToList = mkIso f g where
            f (Just a) = a
            f Nothing = []
            g [] = Nothing
            g a = Just a

stripSpace :: SynMonad t s => Syntax.SynIso t String String
stripSpace = mkIso f g where
    f [] = []
    f (' ':xs) = f xs
    f (x:xs) = x : f xs
    g x = x

mkSynonymIso :: (Eq a, Show a, Eq b, Show b,SyntaxState s) => [(a,b)] -> SynIso s a b
mkSynonymIso ls = Iso f g where
    f e = case snd `fmap` find (\(a,b) -> a == e) ls of
            Just r -> pure r
            Nothing -> lift $ Left $ "No synoyme for " ++ show e
    g e = case fst `fmap` find (\(a,b) -> b == e) ls of
            Just r -> pure r
            Nothing -> lift $ Left $ "No synoyme for " ++ show e

-------------------------------------------------------------------------------
--Type Util
-------------------------------------------------------------------------------

adtLeaf :: SyntaxState s => SynIso s String ADT
adtLeaf = Iso f g where
    f s = pure $ Leaf s
    g (Leaf s) = pure s
    g a = lift $ Left "Not a leaf."

wrapLeaf :: SyntaxState s => SynIso s a String -> SynIso s a [ADT]
wrapLeaf syntax = tolist1 . adtLeaf . syntax

adtNode :: SyntaxState s => SynIso s (String,[ADT]) ADT
adtNode = Iso f g where
    f (s,ls) = pure $ Node s ls
    g (Node s ls) = pure (s,ls)
    g _ = lift $ Left "Not a adtNode."

adtSyntax :: SyntaxState s => String -> SynIso s [ADT] [ADT]
adtSyntax string = tolist1 . adtNode . addfst string

-------------------------------------------------------------------------------
--Syntax Util
-------------------------------------------------------------------------------

letter, digit :: SyntaxState s => Syntax s Char
letter = token (\x -> isLetter x || x=='\'' || x=='.')
digit  = token isDigit

anyWord :: SyntaxState s => Syntax s String
anyWord = some letter <&& sepSpace

any_word :: SyntaxState s => Syntax s [ADT]
any_word = wrapLeaf anyWord

anything :: SyntaxState s => Syntax s a
anything = Iso f g where
    f _ = lift $ Left "Can't parse anything don't know when to stop"
    g _ = lift $ Left "Can't print anything don't know when to stop"

word :: SyntaxState s => String -> Syntax s String
word s = string s <&& sepSpace

mytext :: SyntaxState s => String -> Syntax s ()
mytext s = text s <&& sepSpace

--For text that is both optional and should be parsed into ()
optext :: SyntaxState s => String -> Syntax s ()
optext t = (text t <&& sepSpace) <+> (text "" <&& optSpace)

--Handles 1 of many options from a list
oneOfS :: SyntaxState s => (a -> Syntax s b) -> [a] -> Syntax s b
oneOfS f = foldr ((<+>) . f) zeroArrow

gismu :: SyntaxState s => Syntax s String
gismu = Iso f f . anyWord
    where f word = do
                gismu <- asks gismus
                if TS.member word gismu
                    then pure word
                    else lift $ Left ("'" ++ word ++ "' is not a gismu")

cmene :: SyntaxState s => Syntax s String
cmene = Iso f f . anyWord
    where f word = do
            let char = last word
            if char `notElem` "aeiouy"
               then pure word
               else lift $ Left ("'" ++ word ++ "' is not a cmene.")

adtSelmaho :: SyntaxState s => String -> Syntax s [ADT]
adtSelmaho string = wrapLeaf $ selmaho string

selmaho :: SyntaxState s => String -> Syntax s String
selmaho s = _selmaho s . anyWord

_selmaho :: SyntaxState s => String -> SynIso s String String
_selmaho s = Iso f f
    where f word = do
            cmavo <- asks cmavos
            let selmaho = findWithDefault TS.empty s cmavo
            if TS.member word selmaho
                then pure word
                else lift $ Left $ "'" ++ word ++ "' is not a cmavo of class: " ++ s

sepSelmaho :: SyntaxState s => String -> Syntax s ()
sepSelmaho s = ignoreAny "sepSelmaho FIXME" . selmaho s

optSelmaho :: SyntaxState s => String -> Syntax s ()
optSelmaho s = handle . optional (selmaho s)
    where handle = Iso f g where
            f _ = pure ()
            g () = pure Nothing
