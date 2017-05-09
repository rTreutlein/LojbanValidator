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
text = adtSyntax "text" <<< concatMany ((adtSelmaho "NAI")) &+& concatMany ((wrapLeaf cmene) &+& listoptional (concatSome free)
    <+> (indicators <&> concatSome free)) &+& listoptional joik_jek &+& text_1

text_1 :: SyntaxState s => Syntax s [ADT]
text_1 = adtSyntax "text_1" <<< concatMany (((adtSelmaho "I" &+& listoptional (jek
    <+> joik) &+& listoptional (listoptional stag &+& adtSelmaho "BO") &+& listoptional (concatSome free)))
    <+> concatSome (adtSelmaho "NIhO") &+& listoptional (concatSome free)) &+& listoptional paragraphs

paragraphs :: SyntaxState s => Syntax s [ADT]
paragraphs = adtSyntax "paragraphs" <<< paragraph &+& concatMany ((adtSelmaho "NIhO") &+& listoptional (concatSome free) &+& paragraphs)

paragraph :: SyntaxState s => Syntax s [ADT]
paragraph = adtSyntax "paragraph" <<< (statement
    <+> fragment) &+& concatMany ((adtSelmaho "I" &+& listoptional (concatSome free) &+& listoptional (statement
    <+> fragment)))

statement :: SyntaxState s => Syntax s [ADT]
statement = adtSyntax "statement" <<< statement_1
    <+> prenex &+& statement

statement_1 :: SyntaxState s => Syntax s [ADT]
statement_1 = adtSyntax "statement_1" <<< statement_2 &+& concatMany ((adtSelmaho "I" &+& joik_jek &+& listoptional statement_2))

statement_2 :: SyntaxState s => Syntax s [ADT]
statement_2 = adtSyntax "statement_2" <<< statement_3 &+& listoptional (adtSelmaho "I" &+& listoptional (jek
    <+> joik) &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& listoptional statement_2)

statement_3 :: SyntaxState s => Syntax s [ADT]
statement_3 = adtSyntax "statement_3" <<< sentence
    <+> listoptional tag &+& adtSelmaho "TUhE" &+& listoptional (concatSome free) &+& text_1 &+& listoptional (adtSelmaho "TUhU" &+& listoptional (concatSome free))

fragment :: SyntaxState s => Syntax s [ADT]
fragment = adtSyntax "fragment" <<< ek &+& listoptional (concatSome free)
    <+> gihek &+& listoptional (concatSome free)
    <+> quantifier
    <+> adtSelmaho "NA" &+& listoptional (concatSome free)
    <+> terms &+& listoptional (adtSelmaho "VAU" &+& listoptional (concatSome free))
    <+> prenex
    <+> relative_clauses
    <+> links
    <+> linkargs

prenex :: SyntaxState s => Syntax s [ADT]
prenex = adtSyntax "prenex" <<< terms &+& adtSelmaho "ZOhU" &+& listoptional (concatSome free)

sentence :: SyntaxState s => Syntax s [ADT]
sentence = adtSyntax "sentence" <<< listoptional (terms &+& listoptional (adtSelmaho "CU" &+& listoptional (concatSome free))) &+& bridi_tail

subsentence :: SyntaxState s => Syntax s [ADT]
subsentence = adtSyntax "subsentence" <<< sentence
    <+> prenex &+& subsentence

bridi_tail :: SyntaxState s => Syntax s [ADT]
bridi_tail = adtSyntax "bridi_tail" <<< bridi_tail_1 &+& listoptional (gihek &+& listoptional stag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& bridi_tail &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free)) &+& tail_terms)

bridi_tail_1 :: SyntaxState s => Syntax s [ADT]
bridi_tail_1 = adtSyntax "bridi_tail_1" <<< bridi_tail_2 &+& concatMany ((gihek &+& listoptional (concatSome free) &+& bridi_tail_2 &+& tail_terms))

bridi_tail_2 :: SyntaxState s => Syntax s [ADT]
bridi_tail_2 = adtSyntax "bridi_tail_2" <<< bridi_tail_3 &+& listoptional (gihek &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& bridi_tail_2 &+& tail_terms)

bridi_tail_3 :: SyntaxState s => Syntax s [ADT]
bridi_tail_3 = adtSyntax "bridi_tail_3" <<< selbri &+& tail_terms
    <+> gek_sentence

gek_sentence :: SyntaxState s => Syntax s [ADT]
gek_sentence = adtSyntax "gek_sentence" <<< gek &+& subsentence &+& gik &+& subsentence &+& tail_terms
    <+> listoptional tag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& gek_sentence &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free))
    <+> adtSelmaho "NA" &+& listoptional (concatSome free) &+& gek_sentence

tail_terms :: SyntaxState s => Syntax s [ADT]
tail_terms = adtSyntax "tail_terms" <<< listoptional terms &+& listoptional (adtSelmaho "VAU" &+& listoptional (concatSome free))

terms :: SyntaxState s => Syntax s [ADT]
terms = adtSyntax "terms" <<< concatSome terms_1

terms_1 :: SyntaxState s => Syntax s [ADT]
terms_1 = adtSyntax "terms_1" <<< terms_2 &+& concatMany ((adtSelmaho "PEhE" &+& listoptional (concatSome free) &+& joik_jek &+& terms_2))

terms_2 :: SyntaxState s => Syntax s [ADT]
terms_2 = adtSyntax "terms_2" <<< term &+& concatMany ((adtSelmaho "CEhE" &+& listoptional (concatSome free) &+& term))

term :: SyntaxState s => Syntax s [ADT]
term = adtSyntax "term" <<< sumti
    <+> (tag
    <+> adtSelmaho "FA" &+& listoptional (concatSome free)) &+& (sumti
    <+> listoptional (adtSelmaho "KU" &+& listoptional (concatSome free)))
    <+> termset
    <+> adtSelmaho "NA" &+& adtSelmaho "KU" &+& listoptional (concatSome free)

termset :: SyntaxState s => Syntax s [ADT]
termset = adtSyntax "termset" <<< adtSelmaho "NUhI" &+& listoptional (concatSome free) &+& gek &+& terms &+& listoptional (adtSelmaho "NUhU" &+& listoptional (concatSome free)) &+& gik &+& terms &+& listoptional (adtSelmaho "NUhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "NUhI" &+& listoptional (concatSome free) &+& terms &+& listoptional (adtSelmaho "NUhU" &+& listoptional (concatSome free))

sumti :: SyntaxState s => Syntax s [ADT]
sumti = adtSyntax "sumti" <<< sumti_1 &+& listoptional (adtSelmaho "VUhO" &+& listoptional (concatSome free) &+& relative_clauses)

sumti_1 :: SyntaxState s => Syntax s [ADT]
sumti_1 = adtSyntax "sumti_1" <<< sumti_2 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& sumti &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free)))

sumti_2 :: SyntaxState s => Syntax s [ADT]
sumti_2 = adtSyntax "sumti_2" <<< sumti_3 &+& concatMany ((joik_ek &+& sumti_3))

sumti_3 :: SyntaxState s => Syntax s [ADT]
sumti_3 = adtSyntax "sumti_3" <<< sumti_4 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& sumti_3)

sumti_4 :: SyntaxState s => Syntax s [ADT]
sumti_4 = adtSyntax "sumti_4" <<< sumti_5
    <+> gek &+& sumti &+& gik &+& sumti_4

sumti_5 :: SyntaxState s => Syntax s [ADT]
sumti_5 = adtSyntax "sumti_5" <<< listoptional quantifier &+& sumti_6 &+& listoptional relative_clauses
    <+> quantifier &+& selbri &+& listoptional (adtSelmaho "KU" &+& listoptional (concatSome free)) &+& listoptional relative_clauses

sumti_6 :: SyntaxState s => Syntax s [ADT]
sumti_6 = adtSyntax "sumti_6" <<<
    (adtSelmaho "LAhE" &+& listoptional (concatSome free) <+> adtSelmaho "NAhE" &+& adtSelmaho "BO" &+& listoptional (concatSome free)) &+& listoptional relative_clauses &+& sumti &+& listoptional (adtSelmaho "LUhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "KOhA" &+& listoptional (concatSome free)
    <+> lerfu_string &+& listoptional (adtSelmaho "BOI" &+& listoptional (concatSome free))
    <+> adtSelmaho "LA" &+& listoptional (concatSome free) &+& listoptional relative_clauses &+& concatSome (wrapLeaf cmene) &+& listoptional (concatSome free)
    <+> (adtSelmaho "LA"
    <+> adtSelmaho "LE") &+& listoptional (concatSome free) &+& sumti_tail &+& listoptional (adtSelmaho "KU" &+& listoptional (concatSome free))
    <+> adtSelmaho "LI" &+& listoptional (concatSome free) &+& mex &+& listoptional (adtSelmaho "LOhO" &+& listoptional (concatSome free))
    <+> adtSelmaho "ZO" &+& any_word &+& listoptional (concatSome free)
    <+> adtSelmaho "LU" &+& text &+& listoptional (adtSelmaho "LIhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "LOhU" &+& concatSome any_word &+& adtSelmaho "LEhU" &+& listoptional (concatSome free)
    <+> adtSelmaho "ZOI" &+& handleZOI &+& listoptional (concatSome free)

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
sumti_tail = adtSyntax "sumti_tail" <<< listoptional (sumti_6 &+& listoptional relative_clauses) &+& sumti_tail_1
    <+> relative_clauses &+& sumti_tail_1

sumti_tail_1 :: SyntaxState s => Syntax s [ADT]
sumti_tail_1 = adtSyntax "sumti_tail_1" <<< listoptional quantifier &+& selbri &+& listoptional relative_clauses
    <+> quantifier &+& sumti

relative_clauses :: SyntaxState s => Syntax s [ADT]
relative_clauses = adtSyntax "relative_clauses" <<< relative_clause &+& concatMany ((adtSelmaho "ZIhE" &+& listoptional (concatSome free) &+& relative_clause))

relative_clause :: SyntaxState s => Syntax s [ADT]
relative_clause = adtSyntax "relative_clause" <<< adtSelmaho "GOI" &+& listoptional (concatSome free) &+& term &+& listoptional (adtSelmaho "GEhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "NOI" &+& listoptional (concatSome free) &+& subsentence &+& listoptional (adtSelmaho "KUhO" &+& listoptional (concatSome free))

selbri :: SyntaxState s => Syntax s [ADT]
selbri = adtSyntax "selbri" <<< listoptional tag &+& selbri_1

selbri_1 :: SyntaxState s => Syntax s [ADT]
selbri_1 = adtSyntax "selbri_1" <<< selbri_2
    <+> adtSelmaho "NA" &+& listoptional (concatSome free) &+& selbri

selbri_2 :: SyntaxState s => Syntax s [ADT]
selbri_2 = adtSyntax "selbri_2" <<< selbri_3 &+& listoptional (adtSelmaho "CO" &+& listoptional (concatSome free) &+& selbri_2)

selbri_3 :: SyntaxState s => Syntax s [ADT]
selbri_3 = adtSyntax "selbri_3" <<< concatSome selbri_4

selbri_4 :: SyntaxState s => Syntax s [ADT]
selbri_4 = adtSyntax "selbri_4" <<< selbri_5 &+& concatMany ((joik_jek &+& selbri_5
    <+> joik &+& listoptional stag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& selbri_3 &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free))))

selbri_5 :: SyntaxState s => Syntax s [ADT]
selbri_5 = adtSyntax "selbri_5" <<< selbri_6 &+& listoptional ((jek
    <+> joik) &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& selbri_5)

selbri_6 :: SyntaxState s => Syntax s [ADT]
selbri_6 = adtSyntax "selbri_6" <<< tanru_unit &+& listoptional (adtSelmaho "BO" &+& listoptional (concatSome free) &+& selbri_6)
    <+> listoptional (adtSelmaho "NAhE" &+& listoptional (concatSome free)) &+& guhek &+& selbri &+& gik &+& selbri_6

tanru_unit :: SyntaxState s => Syntax s [ADT]
tanru_unit = adtSyntax "tanru_unit" <<< tanru_unit_1 &+& concatMany ((adtSelmaho "CEI" &+& listoptional (concatSome free) &+& tanru_unit_1))

tanru_unit_1 :: SyntaxState s => Syntax s [ADT]
tanru_unit_1 = adtSyntax "tanru_unit_1" <<< tanru_unit_2 &+& listoptional linkargs

tanru_unit_2 :: SyntaxState s => Syntax s [ADT]
tanru_unit_2 = adtSyntax "tanru_unit_2" <<< (wrapLeaf gismu) &+& listoptional (concatSome free)
    <+> adtSelmaho "GOhA" &+& listoptional (adtSelmaho "RAhO") &+& listoptional (concatSome free)
    <+> adtSelmaho "KE" &+& listoptional (concatSome free) &+& selbri_3 &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free))
    <+> adtSelmaho "ME" &+& listoptional (concatSome free) &+& sumti &+& listoptional (adtSelmaho "MEhU" &+& listoptional (concatSome free)) &+& listoptional (adtSelmaho "MOI" &+& listoptional (concatSome free))
    <+> (number
    <+> lerfu_string) &+& adtSelmaho "MOI" &+& listoptional (concatSome free)
    <+> adtSelmaho "NUhA" &+& listoptional (concatSome free) &+& mex_operator
    <+> adtSelmaho "SE" &+& listoptional (concatSome free) &+& tanru_unit_2
    <+> adtSelmaho "JAI" &+& listoptional (concatSome free) &+& listoptional tag &+& tanru_unit_2
    <+> any_word &+& concatSome ((adtSelmaho "ZEI" &+& any_word))
    <+> adtSelmaho "NAhE" &+& listoptional (concatSome free) &+& tanru_unit_2
    <+> adtSelmaho "NU" &+& listoptional (adtSelmaho "NAI") &+& listoptional (concatSome free) &+& concatMany ((joik_jek &+& adtSelmaho "NU" &+& listoptional (adtSelmaho "NAI") &+& listoptional (concatSome free))) &+& subsentence &+& listoptional (adtSelmaho "KEI" &+& listoptional (concatSome free))

linkargs :: SyntaxState s => Syntax s [ADT]
linkargs = adtSyntax "linkargs" <<< adtSelmaho "BE" &+& listoptional (concatSome free) &+& term &+& listoptional links &+& listoptional (adtSelmaho "BEhO" &+& listoptional (concatSome free))

links :: SyntaxState s => Syntax s [ADT]
links = adtSyntax "links" <<< adtSelmaho "BEI" &+& listoptional (concatSome free) &+& term &+& listoptional links

quantifier :: SyntaxState s => Syntax s [ADT]
quantifier = adtSyntax "quantifier" <<< number &+& listoptional (adtSelmaho "BOI" &+& listoptional (concatSome free))
    <+> adtSelmaho "VEI" &+& listoptional (concatSome free) &+& mex &+& listoptional (adtSelmaho "VEhO" &+& listoptional (concatSome free))

mex :: SyntaxState s => Syntax s [ADT]
mex = adtSyntax "mex" <<< mex_1 &+& concatMany ((operator &+& mex_1))
    <+> adtSelmaho "FUhA" &+& listoptional (concatSome free) &+& rp_expression

mex_1 :: SyntaxState s => Syntax s [ADT]
mex_1 = adtSyntax "mex_1" <<< mex_2 &+& listoptional (adtSelmaho "BIhE" &+& listoptional (concatSome free) &+& operator &+& mex_1)

mex_2 :: SyntaxState s => Syntax s [ADT]
mex_2 = adtSyntax "mex_2" <<< operand
    <+> listoptional (adtSelmaho "PEhO" &+& listoptional (concatSome free)) &+& operator &+& concatSome mex_2 &+& listoptional (adtSelmaho "KUhE" &+& listoptional (concatSome free))

rp_expression :: SyntaxState s => Syntax s [ADT]
rp_expression = adtSyntax "rp_expression" <<< rp_operand &+& rp_operand &+& operator

rp_operand :: SyntaxState s => Syntax s [ADT]
rp_operand = adtSyntax "rp_operand" <<< operand
    <+> rp_expression

operator :: SyntaxState s => Syntax s [ADT]
operator = adtSyntax "operator" <<< operator_1 &+& concatMany ((joik_jek &+& operator_1
    <+> joik &+& listoptional stag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& operator &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free))))

operator_1 :: SyntaxState s => Syntax s [ADT]
operator_1 = adtSyntax "operator_1" <<< operator_2
    <+> guhek &+& operator_1 &+& gik &+& operator_2
    <+> operator_2 &+& (jek
    <+> joik) &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& operator_1

operator_2 :: SyntaxState s => Syntax s [ADT]
operator_2 = adtSyntax "operator_2" <<< mex_operator
    <+> adtSelmaho "KE" &+& listoptional (concatSome free) &+& operator &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free))

mex_operator :: SyntaxState s => Syntax s [ADT]
mex_operator = adtSyntax "mex_operator" <<< adtSelmaho "SE" &+& listoptional (concatSome free) &+& mex_operator
    <+> adtSelmaho "NAhE" &+& listoptional (concatSome free) &+& mex_operator
    <+> adtSelmaho "MAhO" &+& listoptional (concatSome free) &+& mex &+& listoptional (adtSelmaho "TEhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "NAhU" &+& listoptional (concatSome free) &+& selbri &+& listoptional (adtSelmaho "TEhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "VUhU" &+& listoptional (concatSome free)

operand :: SyntaxState s => Syntax s [ADT]
operand = adtSyntax "operand" <<< operand_1 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtSelmaho "KE" &+& listoptional (concatSome free) &+& operand &+& listoptional (adtSelmaho "KEhE" &+& listoptional (concatSome free)))

operand_1 :: SyntaxState s => Syntax s [ADT]
operand_1 = adtSyntax "operand_1" <<< operand_2 &+& concatMany ((joik_ek &+& operand_2))

operand_2 :: SyntaxState s => Syntax s [ADT]
operand_2 = adtSyntax "operand_2" <<< operand_3 &+& listoptional ((ek
    <+> joik) &+& listoptional stag &+& adtSelmaho "BO" &+& listoptional (concatSome free) &+& operand_2)

operand_3 :: SyntaxState s => Syntax s [ADT]
operand_3 = adtSyntax "operand_3" <<< quantifier
    <+> lerfu_string &+& listoptional (adtSelmaho "BOI" &+& listoptional (concatSome free))
    <+> adtSelmaho "NIhE" &+& listoptional (concatSome free) &+& selbri &+& listoptional (adtSelmaho "TEhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "MOhE" &+& listoptional (concatSome free) &+& sumti &+& listoptional (adtSelmaho "TEhU" &+& listoptional (concatSome free))
    <+> adtSelmaho "JOhI" &+& listoptional (concatSome free) &+& concatSome mex_2 &+& listoptional (adtSelmaho "TEhU" &+& listoptional (concatSome free))
    <+> gek &+& operand &+& gik &+& operand_3
    <+> (adtSelmaho "LAhE" &+& listoptional (concatSome free)
    <+> adtSelmaho "NAhE" &+& adtSelmaho "BO" &+& listoptional (concatSome free)) &+& operand &+& listoptional (adtSelmaho "LUhU" &+& listoptional (concatSome free))

number :: SyntaxState s => Syntax s [ADT]
number = adtSyntax "number" <<< adtSelmaho "PA" &+& concatMany ((adtSelmaho "PA"
    <+> lerfu_word))

lerfu_string :: SyntaxState s => Syntax s [ADT]
lerfu_string = adtSyntax "lerfu_string" <<< lerfu_word &+& concatMany ((adtSelmaho "PA"
    <+> lerfu_word))

lerfu_word :: SyntaxState s => Syntax s [ADT]
lerfu_word = adtSyntax "lerfu_word" <<< adtSelmaho "BY"
    <+> any_word &+& adtSelmaho "BU"
    <+> adtSelmaho "LAU" &+& lerfu_word
    <+> adtSelmaho "TEI" &+& lerfu_string &+& adtSelmaho "FOI"

ek :: SyntaxState s => Syntax s [ADT]
ek = adtSyntax "ek" <<< listoptional (adtSelmaho "NA") &+& listoptional (adtSelmaho "SE") &+& adtSelmaho "A" &+& listoptional (adtSelmaho "NAI")

gihek :: SyntaxState s => Syntax s [ADT]
gihek = adtSyntax "gihek" <<< listoptional (adtSelmaho "NA") &+& listoptional (adtSelmaho "SE") &+& adtSelmaho "GIhA" &+& listoptional (adtSelmaho "NAI")

jek :: SyntaxState s => Syntax s [ADT]
jek = adtSyntax "jek" <<< listoptional (adtSelmaho "NA") &+& listoptional (adtSelmaho "SE") &+& adtSelmaho "JA" &+& listoptional (adtSelmaho "NAI")

joik :: SyntaxState s => Syntax s [ADT]
joik = adtSyntax "joik" <<< listoptional (adtSelmaho "SE") &+& adtSelmaho "JOI" &+& listoptional (adtSelmaho "NAI")
    <+> interval
    <+> adtSelmaho "GAhO" &+& interval &+& adtSelmaho "GAhO"

interval :: SyntaxState s => Syntax s [ADT]
interval = adtSyntax "interval" <<< listoptional (adtSelmaho "SE") &+& adtSelmaho "BIhI" &+& listoptional (adtSelmaho "NAI")

joik_ek :: SyntaxState s => Syntax s [ADT]
joik_ek = adtSyntax "joik_ek" <<< joik &+& listoptional (concatSome free)
    <+> ek &+& listoptional (concatSome free)

joik_jek :: SyntaxState s => Syntax s [ADT]
joik_jek = adtSyntax "joik_jek" <<< joik &+& listoptional (concatSome free)
    <+> jek &+& listoptional (concatSome free)

gek :: SyntaxState s => Syntax s [ADT]
gek = adtSyntax "gek" <<< listoptional (adtSelmaho "SE") &+& adtSelmaho "GA" &+& listoptional (adtSelmaho "NAI") &+& listoptional (concatSome free)
    <+> joik &+& adtSelmaho "GI" &+& listoptional (concatSome free)
    <+> stag &+& gik

guhek :: SyntaxState s => Syntax s [ADT]
guhek = adtSyntax "guhek" <<< listoptional (adtSelmaho "SE") &+& adtSelmaho "GUhA" &+& listoptional (adtSelmaho "NAI") &+& listoptional (concatSome free)

gik :: SyntaxState s => Syntax s [ADT]
gik = adtSyntax "gik" <<< adtSelmaho "GI" &+& listoptional (adtSelmaho "NAI") &+& listoptional (concatSome free)

tag :: SyntaxState s => Syntax s [ADT]
tag = adtSyntax "tag" <<< tense_modal &+& concatMany ((joik_jek &+& tense_modal))

stag :: SyntaxState s => Syntax s [ADT]
stag = adtSyntax "stag" <<< simple_tense_modal &+& concatMany (((jek
    <+> joik) &+& simple_tense_modal))

tense_modal :: SyntaxState s => Syntax s [ADT]
tense_modal = adtSyntax "tense_modal" <<< simple_tense_modal &+& listoptional (concatSome free)
    <+> adtSelmaho "FIhO" &+& listoptional (concatSome free) &+& selbri &+& listoptional (adtSelmaho "FEhU" &+& listoptional (concatSome free))

simple_tense_modal :: SyntaxState s => Syntax s [ADT]
simple_tense_modal = adtSyntax "simple_tense_modal" <<< listoptional (adtSelmaho "NAhE") &+& listoptional (adtSelmaho "SE") &+& adtSelmaho "BAI" &+& listoptional (adtSelmaho "NAI") &+& listoptional (adtSelmaho "KI")
    <+> listoptional (adtSelmaho "NAhE") &+& (time &+& listoptional space
    <+> space &+& listoptional time) <&> adtSelmaho "CAhA" &+& listoptional (adtSelmaho "KI")
    <+> adtSelmaho "KI"
    <+> adtSelmaho "CUhE"

time :: SyntaxState s => Syntax s [ADT]
time = adtSyntax "time" <<< adtSelmaho "ZI" <&> concatSome time_offset <&> adtSelmaho "ZEhA" &+& listoptional (adtSelmaho "PU" &+& listoptional (adtSelmaho "NAI")) <&> concatSome interval_property

time_offset :: SyntaxState s => Syntax s [ADT]
time_offset = adtSyntax "time_offset" <<< adtSelmaho "PU" &+& listoptional (adtSelmaho "NAI") &+& listoptional (adtSelmaho "ZI")

space :: SyntaxState s => Syntax s [ADT]
space = adtSyntax "space" <<< adtSelmaho "VA" <&> concatSome space_offset <&> space_interval <&> (adtSelmaho "MOhI" &+& space_offset)

space_offset :: SyntaxState s => Syntax s [ADT]
space_offset = adtSyntax "space_offset" <<< adtSelmaho "FAhA" &+& listoptional (adtSelmaho "NAI") &+& listoptional (adtSelmaho "VA")

space_interval :: SyntaxState s => Syntax s [ADT]
space_interval = adtSyntax "space_interval" <<< ((adtSelmaho "VEhA" <&> adtSelmaho "VIhA") &+& listoptional (adtSelmaho "FAhA" &+& listoptional (adtSelmaho "NAI"))) <&> space_int_props

space_int_props :: SyntaxState s => Syntax s [ADT]
space_int_props = adtSyntax "space_int_props" <<< concatSome ((adtSelmaho "FEhE" &+& interval_property))

interval_property :: SyntaxState s => Syntax s [ADT]
interval_property = adtSyntax "interval_property" <<< number &+& adtSelmaho "ROI" &+& listoptional (adtSelmaho "NAI")
    <+> adtSelmaho "TAhE" &+& listoptional (adtSelmaho "NAI")
    <+> adtSelmaho "ZAhO" &+& listoptional (adtSelmaho "NAI")

free :: SyntaxState s => Syntax s [ADT]
free = adtSyntax "free" <<< adtSelmaho "SEI" &+& listoptional (concatSome free) &+& listoptional (terms &+& listoptional (adtSelmaho "CU" &+& listoptional (concatSome free))) &+& selbri &+& listoptional (adtSelmaho "SEhU")
    <+> adtSelmaho "SOI" &+& listoptional (concatSome free) &+& sumti &+& listoptional sumti &+& listoptional (adtSelmaho "SEhU")
    <+> vocative &+& listoptional relative_clauses &+& selbri &+& listoptional relative_clauses &+& listoptional (adtSelmaho "DOhU")
    <+> vocative &+& listoptional relative_clauses &+& concatSome (wrapLeaf cmene) &+& listoptional (concatSome free) &+& listoptional relative_clauses &+& listoptional (adtSelmaho "DOhU")
    <+> vocative &+& listoptional sumti &+& listoptional (adtSelmaho "DOhU")
    <+> (number
    <+> lerfu_string) &+& adtSelmaho "MAI"
    <+> adtSelmaho "TO" &+& text &+& listoptional (adtSelmaho "TOI")
    <+> adtSelmaho "XI" &+& listoptional (concatSome free) &+& (number
    <+> lerfu_string) &+& listoptional (adtSelmaho "BOI")
    <+> adtSelmaho "XI" &+& listoptional (concatSome free) &+& adtSelmaho "VEI" &+& listoptional (concatSome free) &+& mex &+& listoptional (adtSelmaho "VEhO")

vocative :: SyntaxState s => Syntax s [ADT]
vocative = adtSyntax "vocative" <<< concatSome ((adtSelmaho "COI" &+& listoptional (adtSelmaho "NAI"))) <&> adtSelmaho "DOI"

indicators :: SyntaxState s => Syntax s [ADT]
indicators = adtSyntax "indicators" <<< listoptional (adtSelmaho "FUhE") &+& concatSome indicator

indicator :: SyntaxState s => Syntax s [ADT]
indicator = adtSyntax "indicator" <<< (adtSelmaho "UI"
    <+> adtSelmaho "CAI") &+& listoptional (adtSelmaho "NAI")
    <+> adtSelmaho "Y"
    <+> adtSelmaho "DAhO"
    <+> adtSelmaho "FUhO"

word :: SyntaxState s => Syntax s [ADT]
word = adtSyntax "word" <<< listoptional (adtSelmaho "BAhE") &+& any_word &+& listoptional indicators

null :: SyntaxState s => Syntax s [ADT]
null = adtSyntax "null" <<< any_word &+& adtSelmaho "SI"
    <+> text &+& adtSelmaho "SU"
