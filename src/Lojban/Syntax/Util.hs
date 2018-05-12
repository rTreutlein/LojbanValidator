{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts           #-}
module Lojban.Syntax.Util where

import Prelude hiding (id,(.),(<*>),(<$>),(*>),(<*),foldl)

import Text.Parsers.Frisby (PM,P,runPeg)

import Data.List.Split (splitOn)
import Data.List (nub,partition,intercalate,find,delete)
import Data.Char (chr,isLetter,isDigit)
import Data.Maybe
import Data.Map (findWithDefault)

import Control.Category
import Control.Arrow hiding (left,right)
import Control.Applicative hiding (many,some,optional)
import Control.Monad
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class

import System.Random

import Iso
import Syntax hiding (SynIso,Syntax)
import qualified Syntax

import Lojban.Syntax.Types
import Lojban.Syntax.Morph

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

mkSynonymIso :: (Eq a, Show a, Eq b, Show b,SyntaxState s)
             => [(a,b)] -> SynIso s a b
mkSynonymIso ls = Iso f g where
    f e = case snd `fmap` find (\(a,b) -> a == e) ls of
            Just r -> pure r
            Nothing -> lift $ Left $ "No synoyme for " ++ show e
    g e = case fst `fmap` find (\(a,b) -> b == e) ls of
            Just r -> pure r
            Nothing -> lift $ Left $ "No synoyme for " ++ show e

--Fails when the Syntax Succeds and the other way arround
--Either the syn succeds then we fail with the zeroArrow
--Or the right . insertc succeds because syn failed then we do nothing
notsyn :: SyntaxState s => Syntax s a -> Syntax s ()
notsyn x = (zeroArrow ||| id) . ((left <<< lookahead x) <+> (right . insert ()))

syn :: SyntaxState s => Syntax s a -> Syntax s String
syn x = handle <<< lookahead x
    where handle = mkIso f g
          f _ = []
          g [] = error "syn g Impossible to implement?"

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

consonant, vowel :: SyntaxState s => Syntax s Char
consonant = oneof "bcdfgjklmnprstvxz"
vowel = oneof "aeiouy"

anyWord :: SyntaxState s => Syntax s String
anyWord = someTill letter (text " ") <&& skipSpace

any_word :: SyntaxState s => Syntax s [ADT]
any_word = wrapLeaf anyWord

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
                gismu <- asks wGismus
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

pegToIso :: SyntaxState s2 => (forall s1. PM s1 (P s1 (Maybe (String,String)))) -> String -> Syntax s2 String
pegToIso peg s = handle <<< anyWord
    where handle = Iso f g
          f w = case runPeg peg (w++" ") of
                    Nothing -> lift $ Left $ "Not a " ++ s
                    Just (res,rest) -> case rest of
                        " " -> pure res
                        _ -> modify (addText rest) >> pure res
          g = error "Deal with addText"

brivla,cmevla :: SyntaxState s => Syntax s String
brivla = pegToIso class_BRIVLA "brivla"
cmevla = pegToIso class_CMEVLA "cmevla"

morph :: SyntaxState s => String -> Syntax s String
morph s = pegToIso (morphology s) s

sepMorph :: SyntaxState s => String -> Syntax s ()
sepMorph s = ignoreAny "sepMorph FIXME" . morph s

optMorph :: SyntaxState s => String -> Syntax s ()
optMorph s = handle . optional (morph s)
    where handle = Iso f g where
            f _ = pure ()
            g () = pure Nothing
