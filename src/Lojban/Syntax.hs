{-# LANGUAGE RankNTypes #-}
module Lojban.Syntax where

import Prelude hiding (id,(.))

import Iso
import Syntax hiding (SynIso,Syntax,text)

import Control.Category
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.RWS

import Lojban.Syntax.Types
import Lojban.Syntax.Util
import Lojban.Syntax.Morph hiding (lojban_word,any_word,gismu)

adtMorph :: SyntaxState s => String -> Syntax s [ADT]
adtMorph string = lojban_word $ adtMorph' string

adtMorph' :: SyntaxState s => String -> Syntax s [ADT]
adtMorph' string = wrapLeaf $ morph string

finalCheck :: SyntaxState s => SynIso s a a
finalCheck = Iso f g where
    f a = do
        text <- gets getText
        if text == ""
           then pure a
           else lift $ Left $ "Incomplete parse: " ++ text
    g = pure

lojban :: SyntaxState s => Syntax s [ADT]
lojban = finalCheck . text

text :: SyntaxState s => Syntax s [ADT]
text = adtSyntax "text"
    <<< concatMany (adtMorph "NAI")
    &+& concatMany (lojban_word (wrapLeaf cmevla)
                    &+& listoptional (concatSome free)
                    <+>
                    (indicators <&> concatSome free)
                   )
    &+& listoptional joik_jek
    &+& text_1

text_1 :: SyntaxState s => Syntax s [ADT]
text_1 = adtSyntax "text_1" <<<
    concatMany (adtMorph "I" &+& listoptional (jek <+> joik)
                             &+& listoptional (listoptional stag &+& adtMorph "BO")
                             &+& listoptional (concatSome free)
                <+>
                concatSome (adtMorph "NIhO") &+& listoptional (concatSome free)
               )
    &+& listoptional paragraphs

paragraphs :: SyntaxState s => Syntax s [ADT]
paragraphs = adtSyntax "paragraphs" <<< paragraph
           &+& concatMany (adtMorph "NIhO" &+& listoptional (concatSome free)
                                           &+& paragraphs)

paragraph :: SyntaxState s => Syntax s [ADT]
paragraph = adtSyntax "paragraph" <<< (statement <+> fragment)
    &+& concatMany (adtMorph "I" &+& listoptional (concatSome free)
                                 &+& listoptional (statement <+> fragment))

statement :: SyntaxState s => Syntax s [ADT]
statement = adtSyntax "statement" <<< statement_1
    <+> prenex &+& statement

statement_1 :: SyntaxState s => Syntax s [ADT]
statement_1 = adtSyntax "statement_1" <<< statement_2
            &+& concatMany (adtMorph "I" &+& joik_jek
                                         &+& listoptional statement_2)

statement_2 :: SyntaxState s => Syntax s [ADT]
statement_2 = adtSyntax "statement_2" <<< statement_3
            &+& listoptional (adtMorph "I" &+& listoptional (jek <+> joik)
                                           &+& listoptional stag
                                           &+& adtMorph "BO"
                                           &+& listoptional (concatSome free)
                                           &+& listoptional statement_2)

statement_3 :: SyntaxState s => Syntax s [ADT]
statement_3 = adtSyntax "statement_3" <<< sentence
    <+> listoptional tag &+& adtMorph "TUhE"
                         &+& listoptional (concatSome free)
                         &+& text_1
                         &+& listoptional (adtMorph "TUhU" &+& listoptional (concatSome free))

fragment :: SyntaxState s => Syntax s [ADT]
fragment = adtSyntax "fragment" <<< ek &+& listoptional (concatSome free)
    <+> gihek &+& listoptional (concatSome free)
    <+> quantifier
    <+> adtMorph "NA" &+& listoptional (concatSome free)
    <+> terms &+& listoptional (adtMorph "VAU" &+& listoptional (concatSome free))
    <+> prenex
    <+> relative_clauses
    <+> links
    <+> linkargs

prenex :: SyntaxState s => Syntax s [ADT]
prenex = adtSyntax "prenex" <<< terms &+& adtMorph "ZOhU"
                                      &+& listoptional (concatSome free)

sentence :: SyntaxState s => Syntax s [ADT]
sentence = adtSyntax "sentence"
    <<< listoptional (terms
                      &+& listoptional (adtMorph "CU"
                                        &+& listoptional (concatSome free)
                                       )
                     )
    &+& bridi_tail

subsentence :: SyntaxState s => Syntax s [ADT]
subsentence = adtSyntax "subsentence"
    <<< sentence
    <+> prenex &+& subsentence

bridi_tail :: SyntaxState s => Syntax s [ADT]
bridi_tail = adtSyntax "bridi_tail"
    <<< bridi_tail_1 &+& listoptional (gihek
                                        &+& listoptional stag
                                        &+& adtMorph "KE"
                                        &+& listoptional (concatSome free)
                                        &+& bridi_tail
                                        &+& listoptional (adtMorph "KEhE"
                                                          &+& listoptional (concatSome free)
                                                         )
                                        &+& tail_terms
                                      )

bridi_tail_1 :: SyntaxState s => Syntax s [ADT]
bridi_tail_1 = adtSyntax "bridi_tail_1"
    <<< bridi_tail_2 &+& concatMany (gihek
                                     &+& listoptional (concatSome free)
                                     &+& bridi_tail_2
                                     &+& tail_terms
                                    )

bridi_tail_2 :: SyntaxState s => Syntax s [ADT]
bridi_tail_2 = adtSyntax "bridi_tail_2"
    <<< bridi_tail_3 &+& listoptional (gihek
                                       &+& listoptional stag
                                       &+& adtMorph "BO"
                                       &+& listoptional (concatSome free)
                                       &+& bridi_tail_2
                                       &+& tail_terms)

bridi_tail_3 :: SyntaxState s => Syntax s [ADT]
bridi_tail_3 = adtSyntax "bridi_tail_3" <<< selbri &+& tail_terms
                                        <+> gek_sentence

gek_sentence :: SyntaxState s => Syntax s [ADT]
gek_sentence = adtSyntax "gek_sentence"
    <<< gek &+& subsentence
            &+& gik
            &+& subsentence
            &+& tail_terms
    <+> listoptional tag &+& adtMorph "KE"
                         &+& listoptional (concatSome free)
                         &+& gek_sentence
                         &+& listoptional (adtMorph "KEhE"
                                           &+& listoptional (concatSome free)
                                          )
    <+> adtMorph "NA" &+& listoptional (concatSome free)
                        &+& gek_sentence

tail_terms :: SyntaxState s => Syntax s [ADT]
tail_terms = adtSyntax "tail_terms" <<< listoptional terms
                                    &+& listoptional (adtMorph "VAU"
                                                      &+& listoptional (concatSome free)
                                                     )

terms :: SyntaxState s => Syntax s [ADT]
terms = adtSyntax "terms" <<< concatSome terms_1

terms_1 :: SyntaxState s => Syntax s [ADT]
terms_1 = adtSyntax "terms_1"
    <<< terms_2 &+& concatMany (adtMorph "PEhE"
                                &+& listoptional (concatSome free)
                                &+& joik_jek
                                &+& terms_2
                               )

terms_2 :: SyntaxState s => Syntax s [ADT]
terms_2 = adtSyntax "terms_2"
    <<< term &+& concatMany (adtMorph "CEhE"
                             &+& listoptional (concatSome free)
                             &+& term
                            )

term :: SyntaxState s => Syntax s [ADT]
term = adtSyntax "term"
    <<< sumti
    <+> (tag <+> (adtMorph "FA"
                  &+& listoptional (concatSome free))
                 )
        &+&
        (sumti <+> listoptional (adtMorph "KU"
                                 &+& listoptional (concatSome free)
                                )
        )
    <+> termset
    <+> adtMorph "NA" &+& adtMorph "KU"
                        &+& listoptional (concatSome free)

termset :: SyntaxState s => Syntax s [ADT]
termset = adtSyntax "termset"
    <<< adtMorph "NUhI" &+& listoptional (concatSome free)
                          &+& gek
                          &+& terms
                          &+& listoptional (adtMorph "NUhU"
                                            &+& listoptional (concatSome free)
                                           )
                          &+& gik
                          &+& terms
                          &+& listoptional (adtMorph "NUhU"
                                            &+& listoptional (concatSome free)
                                           )
    <+> adtMorph "NUhI" &+& listoptional (concatSome free)
                          &+& terms
                          &+& listoptional (adtMorph "NUhU"
                                            &+& listoptional (concatSome free)
                                           )

sumti :: SyntaxState s => Syntax s [ADT]
sumti = adtSyntax "sumti" <<< sumti_1 &+& listoptional (adtMorph "VUhO"
                                                        &+& listoptional (concatSome free)
                                                        &+& relative_clauses
                                                       )

sumti_1 :: SyntaxState s => Syntax s [ADT]
sumti_1 = adtSyntax "sumti_1"
       <<< sumti_2 &+& listoptional ((ek <+> joik)
                                     &+& listoptional stag
                                     &+& adtMorph "KE"
                                     &+& listoptional (concatSome free)
                                     &+& sumti
                                     &+& listoptional (adtMorph "KEhE"
                                                       &+& listoptional (concatSome free)
                                                      )
                                    )

sumti_2 :: SyntaxState s => Syntax s [ADT]
sumti_2 = adtSyntax "sumti_2" <<< sumti_3 &+& concatMany ((joik_ek &+& sumti_3))

sumti_3 :: SyntaxState s => Syntax s [ADT]
sumti_3 = adtSyntax "sumti_3"
    <<< sumti_4 &+& listoptional ((ek <+> joik)
                                  &+& listoptional stag
                                  &+& adtMorph "BO"
                                  &+& listoptional (concatSome free)
                                  &+& sumti_3
                                 )

sumti_4 :: SyntaxState s => Syntax s [ADT]
sumti_4 = adtSyntax "sumti_4"
       <<< sumti_5
       <+> gek &+& sumti &+& gik &+& sumti_4

sumti_5 :: SyntaxState s => Syntax s [ADT]
sumti_5 = adtSyntax "sumti_5"
        <<< listoptional quantifier &+& sumti_6
                                    &+& listoptional relative_clauses
        <+> quantifier &+& selbri
                       &+& listoptional (adtMorph "KU"
                                         &+& listoptional (concatSome free)
                                        )
                       &+& listoptional relative_clauses

sumti_6 :: SyntaxState s => Syntax s [ADT]
sumti_6 = adtSyntax "sumti_6"
    <<< (adtMorph "LAhE" &+& listoptional (concatSome free)
         <+> adtMorph "NAhE" &+& adtMorph "BO"
                               &+& listoptional (concatSome free))
        &+& listoptional relative_clauses &+& sumti
                                          &+& listoptional (adtMorph "LUhU" &+& listoptional (concatSome free))

    <+> adtMorph "KOhA" &+& listoptional (concatSome free)

    <+> lerfu_string &+& listoptional (adtMorph "BOI" &+& listoptional (concatSome free))

    <+> adtMorph "LA" &+& listoptional (concatSome free)
                        &+& listoptional relative_clauses
                        &+& concatSome (lojban_word (wrapLeaf cmevla))
                        &+& listoptional (concatSome free)

    <+> (adtMorph "LA" <+> adtMorph "LE")
            &+& listoptional (concatSome free)
            &+& sumti_tail
            &+& listoptional (adtMorph "KU"
                              &+& listoptional (concatSome free)
                             )
    <+> adtMorph "LI" &+& listoptional (concatSome free)
                        &+& mex
                        &+& listoptional (adtMorph "LOhO" &+& listoptional (concatSome free))
    <+> adtMorph "ZO" &+& any_word
                        &+& listoptional (concatSome free)
    <+> adtMorph "LU" &+& text
                        &+& listoptional (adtMorph "LIhU" &+& listoptional (concatSome free))
    <+> adtMorph "LOhU" &+& concatSome any_word
                          &+& adtMorph "LEhU"
                          &+& listoptional (concatSome free)
    <+> adtMorph "ZOI" &+& handleZOI
                         &+& listoptional (concatSome free)

handleZOI :: SyntaxState s => Syntax s [ADT]
handleZOI = Iso f g where
    f () = do
        word <- apply anyWord ()
        any <- apply (isoConcat . manyTill any_word (ignore word <<< string word)) ()
        pure $ (Leaf word):(any ++ [Leaf word])
    g (word:any) = do
        unapply any_word [word]
        unapply (concatMany any_word) (init any)
        unapply any_word [word]
        pure ()

sumti_tail :: SyntaxState s => Syntax s [ADT]
sumti_tail = adtSyntax "sumti_tail"
          <<< listoptional (sumti_6 &+& listoptional relative_clauses)
              &+& sumti_tail_1
          <+> relative_clauses &+& sumti_tail_1

sumti_tail_1 :: SyntaxState s => Syntax s [ADT]
sumti_tail_1 = adtSyntax "sumti_tail_1"
            <<< listoptional quantifier &+& selbri
                                        &+& listoptional relative_clauses
            <+> quantifier &+& sumti

relative_clauses :: SyntaxState s => Syntax s [ADT]
relative_clauses = adtSyntax "relative_clauses"
    <<< relative_clause &+& concatMany (adtMorph "ZIhE"
                                        &+& listoptional (concatSome free)
                                        &+& relative_clause
                                       )

relative_clause :: SyntaxState s => Syntax s [ADT]
relative_clause = adtSyntax "relative_clause"
    <<< adtMorph "GOI" &+& listoptional (concatSome free)
                         &+& term
                         &+& listoptional (adtMorph "GEhU"
                                           &+& listoptional (concatSome free)
                                          )
    <+> adtMorph "NOI" &+& listoptional (concatSome free)
                         &+& subsentence
                         &+& listoptional (adtMorph "KUhO"
                                           &+& listoptional (concatSome free)
                                          )
selbri :: SyntaxState s => Syntax s [ADT]
selbri = adtSyntax "selbri" <<< listoptional tag &+& selbri_1

selbri_1 :: SyntaxState s => Syntax s [ADT]
selbri_1 = adtSyntax "selbri_1" <<< selbri_2
    <+> adtMorph "NA" &+& listoptional (concatSome free) &+& selbri

selbri_2 :: SyntaxState s => Syntax s [ADT]
selbri_2 = adtSyntax "selbri_2" <<< selbri_3 &+& listoptional (adtMorph "CO" &+& listoptional (concatSome free) &+& selbri_2)

selbri_3 :: SyntaxState s => Syntax s [ADT]
selbri_3 = adtSyntax "selbri_3" <<< concatSome selbri_4

selbri_4 :: SyntaxState s => Syntax s [ADT]
selbri_4 = adtSyntax "selbri_4" <<< selbri_5 &+& concatMany ((joik_jek &+& selbri_5
    <+> joik &+& listoptional stag &+& adtMorph "KE" &+& listoptional (concatSome free) &+& selbri_3 &+& listoptional (adtMorph "KEhE" &+& listoptional (concatSome free))))

selbri_5 :: SyntaxState s => Syntax s [ADT]
selbri_5 = adtSyntax "selbri_5" <<< selbri_6 &+& listoptional ((jek
    <+> joik) &+& listoptional stag &+& adtMorph "BO" &+& listoptional (concatSome free) &+& selbri_5)

selbri_6 :: SyntaxState s => Syntax s [ADT]
selbri_6 = adtSyntax "selbri_6" <<< tanru_unit &+& listoptional (adtMorph "BO" &+& listoptional (concatSome free) &+& selbri_6)
    <+> listoptional (adtMorph "NAhE" &+& listoptional (concatSome free)) &+& guhek &+& selbri &+& gik &+& selbri_6

tanru_unit :: SyntaxState s => Syntax s [ADT]
tanru_unit = adtSyntax "tanru_unit" <<< tanru_unit_1
                                    &+& concatMany ((adtMorph "CEI"
                                                        &+& listoptional (concatSome free)
                                                        &+& tanru_unit_1))

tanru_unit_1 :: SyntaxState s => Syntax s [ADT]
tanru_unit_1 = adtSyntax "tanru_unit_1" <<< tanru_unit_2 &+& listoptional linkargs

tanru_unit_2 :: SyntaxState s => Syntax s [ADT]
tanru_unit_2 = adtSyntax "tanru_unit_2" <<<
    lojban_word (wrapLeaf (gismu <+> brivla)) &+& listoptional (concatSome free)
    <+> adtMorph "GOhA" &+& listoptional (adtMorph "RAhO")
                          &+& listoptional (concatSome free)

    <+> adtMorph "KE" &+& listoptional (concatSome free)
                        &+& selbri_3
                        &+& listoptional (adtMorph "KEhE"
                                          &+& listoptional (concatSome free))

    <+> adtMorph "ME" &+& listoptional (concatSome free)
                        &+& sumti
                        &+& listoptional (adtMorph "MEhU"
                                          &+& listoptional (concatSome free))
                        &+& listoptional (adtMorph "MOI"
                                          &+& listoptional (concatSome free))

    <+> (number <+> lerfu_string) &+& adtMorph "MOI"
                                  &+& listoptional (concatSome free)

    <+> adtMorph "NUhA" &+& listoptional (concatSome free)
                        &+& mex_operator
    <+> adtMorph "SE" &+& listoptional (concatSome free)
                      &+& tanru_unit_2
    <+> adtMorph "JAI" &+& listoptional (concatSome free)
                       &+& listoptional tag
                       &+& tanru_unit_2
    <+> any_word &+& concatSome ((adtMorph "ZEI" &+& any_word))
    <+> adtMorph "NAhE" &+& listoptional (concatSome free)
                        &+& tanru_unit_2
    <+> adtMorph "NU" &+& listoptional (adtMorph "NAI")
                      &+& listoptional (concatSome free)
                      &+& concatMany ((joik_jek &+& adtMorph "NU"
                                                &+& listoptional (adtMorph "NAI")
                                                &+& listoptional (concatSome free)))
                      &+& subsentence
                      &+& listoptional (adtMorph "KEI"
                                        &+& listoptional (concatSome free))

linkargs :: SyntaxState s => Syntax s [ADT]
linkargs = adtSyntax "linkargs"
    <<< adtMorph "BE" &+& listoptional (concatSome free)
                        &+& term
                        &+& listoptional links
                        &+& listoptional (adtMorph "BEhO"
                                          &+& listoptional (concatSome free)
                                         )

links :: SyntaxState s => Syntax s [ADT]
links = adtSyntax "links"
    <<< adtMorph "BEI" &+& listoptional (concatSome free)
                         &+& term
                         &+& listoptional links

quantifier :: SyntaxState s => Syntax s [ADT]
quantifier = adtSyntax "quantifier"
          <<< number &+& listoptional (adtMorph "BOI"
                                       &+& listoptional (concatSome free)
                                      )
          <+> adtMorph "VEI" &+& listoptional (concatSome free)
                               &+& mex
                               &+& listoptional (adtMorph "VEhO"
                                                 &+& listoptional (concatSome free)
                                                )

mex :: SyntaxState s => Syntax s [ADT]
mex = adtSyntax "mex" <<< mex_1 &+& concatMany ((operator &+& mex_1))
    <+> adtMorph "FUhA" &+& listoptional (concatSome free) &+& rp_expression

mex_1 :: SyntaxState s => Syntax s [ADT]
mex_1 = adtSyntax "mex_1" <<< mex_2 &+& listoptional (adtMorph "BIhE" &+& listoptional (concatSome free) &+& operator &+& mex_1)

mex_2 :: SyntaxState s => Syntax s [ADT]
mex_2 = adtSyntax "mex_2" <<< operand
    <+> listoptional (adtMorph "PEhO" &+& listoptional (concatSome free)) &+& operator &+& concatSome mex_2 &+& listoptional (adtMorph "KUhE" &+& listoptional (concatSome free))

rp_expression :: SyntaxState s => Syntax s [ADT]
rp_expression = adtSyntax "rp_expression" <<< rp_operand &+& rp_operand &+& operator

rp_operand :: SyntaxState s => Syntax s [ADT]
rp_operand = adtSyntax "rp_operand" <<< operand
    <+> rp_expression

operator :: SyntaxState s => Syntax s [ADT]
operator = adtSyntax "operator" <<< operator_1 &+& concatMany ((joik_jek &+& operator_1
    <+> joik &+& listoptional stag &+& adtMorph "KE" &+& listoptional (concatSome free) &+& operator &+& listoptional (adtMorph "KEhE" &+& listoptional (concatSome free))))

operator_1 :: SyntaxState s => Syntax s [ADT]
operator_1 = adtSyntax "operator_1" <<< operator_2
    <+> guhek &+& operator_1 &+& gik &+& operator_2
    <+> operator_2 &+& (jek
    <+> joik) &+& listoptional stag &+& adtMorph "BO" &+& listoptional (concatSome free) &+& operator_1

operator_2 :: SyntaxState s => Syntax s [ADT]
operator_2 = adtSyntax "operator_2" <<< mex_operator
    <+> adtMorph "KE" &+& listoptional (concatSome free) &+& operator &+& listoptional (adtMorph "KEhE" &+& listoptional (concatSome free))

mex_operator :: SyntaxState s => Syntax s [ADT]
mex_operator = adtSyntax "mex_operator" <<< adtMorph "SE" &+& listoptional (concatSome free) &+& mex_operator
    <+> adtMorph "NAhE" &+& listoptional (concatSome free) &+& mex_operator
    <+> adtMorph "MAhO" &+& listoptional (concatSome free) &+& mex &+& listoptional (adtMorph "TEhU" &+& listoptional (concatSome free))
    <+> adtMorph "NAhU" &+& listoptional (concatSome free) &+& selbri &+& listoptional (adtMorph "TEhU" &+& listoptional (concatSome free))
    <+> adtMorph "VUhU" &+& listoptional (concatSome free)

operand :: SyntaxState s => Syntax s [ADT]
operand = adtSyntax "operand" <<< operand_1 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtMorph "KE" &+& listoptional (concatSome free) &+& operand &+& listoptional (adtMorph "KEhE" &+& listoptional (concatSome free)))

operand_1 :: SyntaxState s => Syntax s [ADT]
operand_1 = adtSyntax "operand_1" <<< operand_2 &+& concatMany ((joik_ek &+& operand_2))

operand_2 :: SyntaxState s => Syntax s [ADT]
operand_2 = adtSyntax "operand_2" <<< operand_3 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtMorph "BO" &+& listoptional (concatSome free) &+& operand_2)

operand_3 :: SyntaxState s => Syntax s [ADT]
operand_3 = adtSyntax "operand_3"
    <<< quantifier
    <+> lerfu_string &+& listoptional (adtMorph "BOI"
                                       &+& listoptional (concatSome free)
                                      )
    <+> adtMorph "NIhE" &+& listoptional (concatSome free)
                          &+& selbri
                          &+& listoptional (adtMorph "TEhU"
                                            &+& listoptional (concatSome free)
                                           )
    <+> adtMorph "MOhE" &+& listoptional (concatSome free)
                          &+& sumti
                          &+& listoptional (adtMorph "TEhU"
                                            &+& listoptional (concatSome free)
                                           )
    <+> adtMorph "JOhI" &+& listoptional (concatSome free)
                          &+& concatSome mex_2
                          &+& listoptional (adtMorph "TEhU"
                                            &+& listoptional (concatSome free)
                                           )
    <+> gek &+& operand
            &+& gik
            &+& operand_3
    <+> (adtMorph "LAhE" &+& listoptional (concatSome free)
         <+> adtMorph "NAhE" &+& adtMorph "BO"
                               &+& listoptional (concatSome free)
        )
        &+& operand
        &+& listoptional (adtMorph "LUhU"
                          &+& listoptional (concatSome free)
                         )

number :: SyntaxState s => Syntax s [ADT]
number = adtSyntax "number"
      <<< adtMorph "PA" &+& concatMany ((adtMorph "PA" <+> lerfu_word))

lerfu_string :: SyntaxState s => Syntax s [ADT]
lerfu_string = adtSyntax "lerfu_string" <<< lerfu_word &+& concatMany ((adtMorph "PA"
    <+> lerfu_word))

lerfu_word :: SyntaxState s => Syntax s [ADT]
lerfu_word = adtSyntax "lerfu_word"
    <<< adtMorph "BY"
    <+> any_word &+& adtMorph "BU"
    <+> adtMorph "LAU" &+& lerfu_word
    <+> adtMorph "TEI" &+& lerfu_string &+& adtMorph "FOI"

ek :: SyntaxState s => Syntax s [ADT]
ek = adtSyntax "ek" <<< listoptional (adtMorph "NA") &+& listoptional (adtMorph "SE") &+& adtMorph "A" &+& listoptional (adtMorph "NAI")

gihek :: SyntaxState s => Syntax s [ADT]
gihek = adtSyntax "gihek" <<< listoptional (adtMorph "NA") &+& listoptional (adtMorph "SE") &+& adtMorph "GIhA" &+& listoptional (adtMorph "NAI")

jek :: SyntaxState s => Syntax s [ADT]
jek = adtSyntax "jek" <<< listoptional (adtMorph "NA") &+& listoptional (adtMorph "SE") &+& adtMorph "JA" &+& listoptional (adtMorph "NAI")

joik :: SyntaxState s => Syntax s [ADT]
joik = adtSyntax "joik" <<< listoptional (adtMorph "SE") &+& adtMorph "JOI" &+& listoptional (adtMorph "NAI")
    <+> interval
    <+> adtMorph "GAhO" &+& interval &+& adtMorph "GAhO"

interval :: SyntaxState s => Syntax s [ADT]
interval = adtSyntax "interval" <<< listoptional (adtMorph "SE") &+& adtMorph "BIhI" &+& listoptional (adtMorph "NAI")

joik_ek :: SyntaxState s => Syntax s [ADT]
joik_ek = adtSyntax "joik_ek" <<< joik &+& listoptional (concatSome free)
    <+> ek &+& listoptional (concatSome free)

joik_jek :: SyntaxState s => Syntax s [ADT]
joik_jek = adtSyntax "joik_jek" <<< joik &+& listoptional (concatSome free)
    <+> jek &+& listoptional (concatSome free)

gek :: SyntaxState s => Syntax s [ADT]
gek = adtSyntax "gek" <<< listoptional (adtMorph "SE") &+& adtMorph "GA" &+& listoptional (adtMorph "NAI") &+& listoptional (concatSome free)
    <+> joik &+& adtMorph "GI" &+& listoptional (concatSome free)
    <+> stag &+& gik

guhek :: SyntaxState s => Syntax s [ADT]
guhek = adtSyntax "guhek" <<< listoptional (adtMorph "SE") &+& adtMorph "GUhA" &+& listoptional (adtMorph "NAI") &+& listoptional (concatSome free)

gik :: SyntaxState s => Syntax s [ADT]
gik = adtSyntax "gik" <<< adtMorph "GI" &+& listoptional (adtMorph "NAI") &+& listoptional (concatSome free)

tag :: SyntaxState s => Syntax s [ADT]
tag = adtSyntax "tag" <<< tense_modal &+& concatMany ((joik_jek &+& tense_modal))

stag :: SyntaxState s => Syntax s [ADT]
stag = adtSyntax "stag" <<< simple_tense_modal &+& concatMany (((jek
    <+> joik) &+& simple_tense_modal))

tense_modal :: SyntaxState s => Syntax s [ADT]
tense_modal = adtSyntax "tense_modal" <<<
    simple_tense_modal &+& listoptional (concatSome free)
    <+> adtMorph "FIhO" &+& listoptional (concatSome free)
                          &+& selbri
                          &+& listoptional (adtMorph "FEhU" &+&
                                            listoptional (concatSome free))

simple_tense_modal :: SyntaxState s => Syntax s [ADT]
simple_tense_modal = adtSyntax "simple_tense_modal" <<<
    listoptional (adtMorph "NAhE") &+& listoptional (adtMorph "SE")
                                     &+& adtMorph "BAI"
                                     &+& listoptional (adtMorph "NAI")
                                     &+& listoptional (adtMorph "KI")
    <+> listoptional (adtMorph "NAhE") &+&
        (time &+& listoptional space <+> space &+& listoptional time)
        <&> adtMorph "CAhA" &+& listoptional (adtMorph "KI")
    <+> adtMorph "KI"
    <+> adtMorph "CUhE"

time :: SyntaxState s => Syntax s [ADT]
time = adtSyntax "time" <<<
    adtMorph "ZI"
    <&> concatSome time_offset
    <&> time_interval
    <&> concatSome interval_property

space :: SyntaxState s => Syntax s [ADT]
space = adtSyntax "space" <<<
    adtMorph "VA"
    <&> concatSome space_offset
    <&> space_interval
    <&> (adtMorph "MOhI" &+& space_offset)

time_offset :: SyntaxState s => Syntax s [ADT]
time_offset = adtSyntax "time_offset" <<<
    adtMorph "PU" &+& listoptional (adtMorph "NAI")
                    &+& listoptional (adtMorph "ZI")

space_offset :: SyntaxState s => Syntax s [ADT]
space_offset = adtSyntax "space_offset" <<<
    adtMorph "FAhA" &+& listoptional (adtMorph "NAI")
                      &+& listoptional (adtMorph "VA")

time_interval :: SyntaxState s => Syntax s [ADT]
time_interval = adtSyntax "time_interval" <<<
    adtMorph "ZEhA" &+& listoptional (adtMorph "PU"
                      &+& listoptional (adtMorph "NAI"))

space_interval :: SyntaxState s => Syntax s [ADT]
space_interval = adtSyntax "space_interval" <<<
    ((adtMorph "VEhA" <&> adtMorph "VIhA")
        &+& listoptional (adtMorph "FAhA" &+& listoptional (adtMorph "NAI")))
    <&> space_int_props

space_int_props :: SyntaxState s => Syntax s [ADT]
space_int_props = adtSyntax "space_int_props" <<< concatSome ((adtMorph "FEhE" &+& interval_property))

interval_property :: SyntaxState s => Syntax s [ADT]
interval_property = adtSyntax "interval_property" <<<
    number &+& adtMorph "ROI"
           &+& listoptional (adtMorph "NAI")
    <+> adtMorph "TAhE" &+& listoptional (adtMorph "NAI")
    <+> adtMorph "ZAhO" &+& listoptional (adtMorph "NAI")

free :: SyntaxState s => Syntax s [ADT]
free = adtSyntax "free" <<<
    adtMorph "SEI" &+& listoptional (concatSome free)
                     &+& listoptional (terms
                                       &+& listoptional (adtMorph "CU"
                                                         &+& listoptional (concatSome free)))
                     &+& selbri
                     &+& listoptional (adtMorph "SEhU")
    <+> adtMorph "SOI" &+& listoptional (concatSome free)
                         &+& sumti
                         &+& listoptional sumti
                         &+& listoptional (adtMorph "SEhU")
    <+> vocative &+& listoptional relative_clauses
                 &+& selbri
                 &+& listoptional relative_clauses
                 &+& listoptional (adtMorph "DOhU")

    <+> vocative &+& listoptional relative_clauses
                 &+& concatSome (lojban_word (wrapLeaf cmevla))
                 &+& listoptional (concatSome free)
                 &+& listoptional relative_clauses
                 &+& listoptional (adtMorph "DOhU")

    <+> vocative &+& listoptional sumti
                 &+& listoptional (adtMorph "DOhU")
    <+> (number <+> lerfu_string) &+& adtMorph "MAI"
    <+> adtMorph "TO" &+& text
                        &+& listoptional (adtMorph "TOI")
    <+> adtMorph "XI" &+& listoptional (concatSome free)
                        &+& (number <+> lerfu_string)
                        &+& listoptional (adtMorph "BOI")
    <+> adtMorph "XI" &+& listoptional (concatSome free)
                        &+& adtMorph "VEI"
                        &+& listoptional (concatSome free)
                        &+& mex
                        &+& listoptional (adtMorph "VEhO")

vocative :: SyntaxState s => Syntax s [ADT]
vocative = adtSyntax "vocative" <<< concatSome (adtMorph "COI"
                                                &+& listoptional (adtMorph "NAI")
                                               )
                                <&> adtMorph "DOI"

indicators :: SyntaxState s => Syntax s [ADT]
indicators = adtSyntax "indicators" <<<
    listoptional (adtMorph "FUhE") &+& concatSome indicator

indicator :: SyntaxState s => Syntax s [ADT]
indicator = adtSyntax "indicator" <<<
    (adtMorph "UI" <+> adtMorph "CAI") &+& listoptional (adtMorph "NAI")
    <+> adtMorph "Y"
    <+> adtMorph "DAhO"
    <+> adtMorph "FUhO"

lojban_word :: SyntaxState s => Syntax s [ADT] -> Syntax s [ADT]
lojban_word syn = adtSyntax "word"
    <<< listoptional (adtMorph' "BAhE") &+& syn &+& listoptional indicators

null :: SyntaxState s => Syntax s [ADT]
null = adtSyntax "null" <<< any_word &+& adtMorph "SI"
    <+> text &+& adtMorph "SU"
