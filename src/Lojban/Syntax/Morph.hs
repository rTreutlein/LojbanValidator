{-# LANGUAGE RecursiveDo #-}
module Lojban.Syntax.Morph
    (class_CMEVLA
    ,class_BRIVLA
    ,morphology
    ) where

import Text.Parsers.Frisby hiding (matches,doesNotMatch)
import qualified Text.Parsers.Frisby as FB
import Control.Exception
import System.IO.Unsafe

matches p = peek p ->> unit []

doesNotMatch p = FB.doesNotMatch p ->> unit []

tolist1 a = [a]

concatMany p = concat <$> many p

concatSome p = concat <$> many1 p

--class_BRIVLA = fst <$> morphology
--class_CMEVLA = snd <$> morphology
class_BRIVLA = morphology "brivla"
class_CMEVLA = morphology "cmevla"

morphology _class = mdo
    class_CMEVLA  <- newRule $ cmevla
    class_BRIVLA  <- newRule $ gismu // lujvo // fuhivla
    class_CMAVO  <- newRule $ class_A // class_BAI // class_BAhE // class_BE // class_BEI // class_BEhO // class_BIhE // class_BIhI // class_BO // class_BOI // class_BU // class_BY // class_CAhA // class_CAI // class_CEI // class_CEhE // class_CO // class_COI // class_CU // class_CUhE // class_DAhO // class_DOI // class_DOhU // class_FA // class_FAhA // class_FAhO // class_FEhE // class_FEhU // class_FIhO // class_FOI // class_FUhA // class_FUhE // class_FUhO // class_GA // class_GAhO // class_GEhU // class_GI // class_GIhA // class_GOI // class_GOhA // class_GUhA // class_I // class_JA // class_JAI // class_JOhI // class_JOI // class_KE // class_KEhE // class_KEI // class_KI // class_KOhA // class_KU // class_KUhE // class_KUhO // class_LA // class_LAU // class_LAhE // class_LE // class_LEhU // class_LI // class_LIhU // class_LOhO // class_LOhU // class_LU // class_LUhU // class_MAhO // class_MAI // class_ME // class_MEhU // class_MOhE // class_MOhI // class_MOI // class_NA // class_NAI // class_NAhE // class_NAhU // class_NIhE // class_NIhO // class_NOI // class_NU // class_NUhA // class_NUhI // class_NUhU // class_PA // class_PEhE // class_PEhO // class_PU // class_RAhO // class_ROI // class_SA // class_SE // class_SEI // class_SEhU // class_SI // class_SOI // class_SU // class_TAhE // class_TEhU // class_TEI // class_TO // class_TOI // class_TUhE // class_TUhU // class_UI // class_VA // class_VAU // class_VEI // class_VEhO // class_VUhU // class_VEhA // class_VIhA // class_VUhO // class_XI // class_ZAhO // class_ZEhA // class_ZEI // class_ZI // class_ZIhE // class_ZO // class_ZOI // class_ZOhU // cmavo
    lojban_word  <- newRule $ class_CMEVLA // class_CMAVO // class_BRIVLA
    any_word  <- newRule $ lojban_word <++> option [] (spaces)
    zoi_open  <- newRule $ lojban_word
    zoi_word  <- newRule $ concatSome (non_space)
    zoi_close  <- newRule $ any_word
    cmevla  <- newRule $ jbocme // zifcme
    zifcme  <- newRule $ doesNotMatch (h) <++> concatMany ((nucleus // glide // h // consonant <++> doesNotMatch (pause) // digit)) <++> consonant <++> matches (pause)
    jbocme  <- newRule $ matches (zifcme) <++> concatMany ((any_syllable // digit)) <++> matches (pause)
    cmavo  <- newRule $ doesNotMatch (cmevla) <++> doesNotMatch (class_CVCy_lujvo) <++> cmavo_form <++> matches (post_word)
    class_CVCy_lujvo  <- newRule $ class_CVC_rafsi <++> y <++> option [] (h) <++> concatMany (initial_rafsi) <++> brivla_core // stressed_CVC_rafsi <++> y <++> short_final_rafsi
    cmavo_form  <- newRule $ doesNotMatch (h) <++> doesNotMatch (cluster) <++> onset <++> concatMany ((nucleus <++> h)) <++> (doesNotMatch (stressed) <++> nucleus // nucleus <++> doesNotMatch (cluster)) // concatSome (y) // digit
    brivla  <- newRule $ doesNotMatch (cmavo) <++> concatMany (initial_rafsi) <++> brivla_core
    brivla_core  <- newRule $ fuhivla // gismu // class_CVV_final_rafsi // stressed_initial_rafsi <++> short_final_rafsi
    stressed_initial_rafsi  <- newRule $ stressed_extended_rafsi // stressed_y_rafsi // stressed_y_less_rafsi
    initial_rafsi  <- newRule $ extended_rafsi // y_rafsi // doesNotMatch (any_extended_rafsi) <++> y_less_rafsi <++> doesNotMatch (any_extended_rafsi)
    any_extended_rafsi  <- newRule $ fuhivla // extended_rafsi // stressed_extended_rafsi
    fuhivla  <- newRule $ fuhivla_head <++> stressed_syllable <++> concatMany (consonantal_syllable) <++> final_syllable
    stressed_extended_rafsi  <- newRule $ stressed_brivla_rafsi // stressed_fuhivla_rafsi
    extended_rafsi  <- newRule $ brivla_rafsi // fuhivla_rafsi
    stressed_brivla_rafsi  <- newRule $ matches (unstressed_syllable) <++> brivla_head <++> stressed_syllable <++> h <++> y
    brivla_rafsi  <- newRule $ matches ((syllable <++> concatMany (consonantal_syllable) <++> syllable)) <++> brivla_head <++> h <++> y <++> option [] (h)
    stressed_fuhivla_rafsi  <- newRule $ fuhivla_head <++> stressed_syllable <++> concatMany (consonantal_syllable) <++> doesNotMatch (h) <++> onset <++> y
    fuhivla_rafsi  <- newRule $ matches (unstressed_syllable) <++> fuhivla_head <++> doesNotMatch (h) <++> onset <++> y <++> option [] (h)
    fuhivla_head  <- newRule $ doesNotMatch (rafsi_string) <++> brivla_head
    brivla_head  <- newRule $ doesNotMatch (cmavo) <++> doesNotMatch (slinkuhi) <++> doesNotMatch (h) <++> matches (onset) <++> concatMany (unstressed_syllable)
    slinkuhi  <- newRule $ doesNotMatch (rafsi_string) <++> consonant <++> rafsi_string
    rafsi_string  <- newRule $ concatMany (y_less_rafsi) <++> (gismu // class_CVV_final_rafsi // stressed_y_less_rafsi <++> short_final_rafsi // y_rafsi // stressed_y_rafsi // option [] (stressed_y_less_rafsi) <++> initial_pair <++> y // hy_rafsi // stressed_hy_rafsi)
    gismu  <- newRule $ (initial_pair <++> stressed_vowel // consonant <++> stressed_vowel <++> consonant) <++> matches (final_syllable) <++> consonant <++> vowel <++> matches (post_word)
    class_CVV_final_rafsi  <- newRule $ consonant <++> stressed_vowel <++> h <++> matches (final_syllable) <++> vowel <++> matches (post_word)
    short_final_rafsi  <- newRule $ matches (final_syllable) <++> (consonant <++> diphthong // initial_pair <++> vowel) <++> matches (post_word)
    stressed_y_rafsi  <- newRule $ (stressed_long_rafsi // stressed_CVC_rafsi) <++> y
    stressed_y_less_rafsi  <- newRule $ stressed_CVC_rafsi <++> doesNotMatch (y) // stressed_CCV_rafsi // stressed_CVV_rafsi
    stressed_long_rafsi  <- newRule $ initial_pair <++> stressed_vowel <++> consonant // consonant <++> stressed_vowel <++> consonant <++> consonant
    stressed_CVC_rafsi  <- newRule $ consonant <++> stressed_vowel <++> consonant
    stressed_CCV_rafsi  <- newRule $ initial_pair <++> stressed_vowel
    stressed_CVV_rafsi  <- newRule $ consonant <++> (unstressed_vowel <++> h <++> stressed_vowel // stressed_diphthong) <++> option [] (r_hyphen)
    y_rafsi  <- newRule $ (long_rafsi // class_CVC_rafsi) <++> y <++> option [] (h)
    y_less_rafsi  <- newRule $ doesNotMatch (y_rafsi) <++> doesNotMatch (stressed_y_rafsi) <++> doesNotMatch (hy_rafsi) <++> doesNotMatch (stressed_hy_rafsi) <++> (class_CVC_rafsi // class_CCV_rafsi // class_CVV_rafsi) <++> doesNotMatch (h)
    hy_rafsi  <- newRule $ (long_rafsi <++> vowel // class_CCV_rafsi // class_CVV_rafsi) <++> h <++> y <++> option [] (h)
    stressed_hy_rafsi  <- newRule $ (long_rafsi <++> stressed_vowel // stressed_CCV_rafsi // stressed_CVV_rafsi) <++> h <++> y
    long_rafsi  <- newRule $ initial_pair <++> unstressed_vowel <++> consonant // consonant <++> unstressed_vowel <++> consonant <++> consonant
    class_CVC_rafsi  <- newRule $ consonant <++> unstressed_vowel <++> consonant
    class_CCV_rafsi  <- newRule $ initial_pair <++> unstressed_vowel
    class_CVV_rafsi  <- newRule $ consonant <++> (unstressed_vowel <++> h <++> unstressed_vowel // unstressed_diphthong) <++> option [] (r_hyphen)
    r_hyphen  <- newRule $ r <++> matches (consonant) // n <++> matches (r)
    final_syllable  <- newRule $ onset <++> doesNotMatch (y) <++> doesNotMatch (stressed) <++> nucleus <++> doesNotMatch (cmevla) <++> matches (post_word)
    stressed_syllable  <- newRule $ matches (stressed) <++> syllable // syllable <++> matches (stress)
    stressed_diphthong  <- newRule $ matches (stressed) <++> diphthong // diphthong <++> matches (stress)
    stressed_vowel  <- newRule $ matches (stressed) <++> vowel // vowel <++> matches (stress)
    unstressed_syllable  <- newRule $ doesNotMatch (stressed) <++> syllable <++> doesNotMatch (stress) // consonantal_syllable
    unstressed_diphthong  <- newRule $ doesNotMatch (stressed) <++> diphthong <++> doesNotMatch (stress)
    unstressed_vowel  <- newRule $ doesNotMatch (stressed) <++> vowel <++> doesNotMatch (stress)
    stress  <- newRule $ concatMany ((consonant // glide)) <++> option [] (h) <++> option [] (y) <++> syllable <++> pause
    stressed  <- newRule $ onset <++> concatMany (comma) <++> (tolist1 <$> oneOf "AEIOU")
    any_syllable  <- newRule $ onset <++> nucleus <++> option [] (coda) // consonantal_syllable
    syllable  <- newRule $ onset <++> doesNotMatch (y) <++> nucleus <++> option [] (coda)
    consonantal_syllable  <- newRule $ consonant <++> matches (syllabic) <++> coda
    coda  <- newRule $ doesNotMatch (any_syllable) <++> consonant <++> matches (any_syllable) // option [] (syllabic) <++> option [] (consonant) <++> matches (pause)
    onset  <- newRule $ h // glide // initial
    nucleus  <- newRule $ vowel // diphthong // y <++> doesNotMatch (nucleus)
    glide  <- newRule $ (i // u) <++> matches (nucleus)
    diphthong  <- newRule $ (a <++> i <++> doesNotMatch (i) // a <++> u <++> doesNotMatch (u) // e <++> i <++> doesNotMatch (i) // o <++> i <++> doesNotMatch (i)) <++> doesNotMatch (nucleus)
    vowel  <- newRule $ (a // e // i // o // u) <++> doesNotMatch (nucleus)
    a  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "aA")
    e  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "eE")
    i  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "iI")
    o  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "oO")
    u  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "uU")
    y  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "yY") <++> doesNotMatch ((doesNotMatch (y) <++> nucleus))
    cluster  <- newRule $ consonant <++> concatSome (consonant)
    initial_pair  <- newRule $ matches (initial) <++> consonant <++> consonant <++> doesNotMatch (consonant)
    initial  <- newRule $ (affricate // option [] (sibilant) <++> option [] (other) <++> option [] (liquid)) <++> doesNotMatch (consonant) <++> doesNotMatch (glide)
    affricate  <- newRule $ t <++> c // t <++> s // d <++> j // d <++> z
    liquid  <- newRule $ l // r
    other  <- newRule $ p // t <++> doesNotMatch (l) // k // f // x // b // d <++> doesNotMatch (l) // g // v // m // n <++> doesNotMatch (liquid)
    sibilant  <- newRule $ c // s <++> doesNotMatch (x) // (j // z) <++> doesNotMatch (n) <++> doesNotMatch (liquid)
    consonant  <- newRule $ voiced // unvoiced // syllabic
    syllabic  <- newRule $ l // m // n // r
    voiced  <- newRule $ b // d // g // j // v // z
    unvoiced  <- newRule $ c // f // k // p // s // t // x
    l  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "lL") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (l)
    m  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "mM") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (m) <++> doesNotMatch (z)
    n  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "nN") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (n) <++> doesNotMatch (affricate)
    r  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "rR") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (r)
    b  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "bB") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (b) <++> doesNotMatch (unvoiced)
    d  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "dD") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (d) <++> doesNotMatch (unvoiced)
    g  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "gG") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (g) <++> doesNotMatch (unvoiced)
    v  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "vV") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (v) <++> doesNotMatch (unvoiced)
    j  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "jJ") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (j) <++> doesNotMatch (z) <++> doesNotMatch (unvoiced)
    z  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "zZ") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (z) <++> doesNotMatch (j) <++> doesNotMatch (unvoiced)
    s  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "sS") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (s) <++> doesNotMatch (c) <++> doesNotMatch (voiced)
    c  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "cC") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (c) <++> doesNotMatch (s) <++> doesNotMatch (x) <++> doesNotMatch (voiced)
    x  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "xX") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (x) <++> doesNotMatch (c) <++> doesNotMatch (k) <++> doesNotMatch (voiced)
    k  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "kK") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (k) <++> doesNotMatch (x) <++> doesNotMatch (voiced)
    f  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "fF") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (f) <++> doesNotMatch (voiced)
    p  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "pP") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (p) <++> doesNotMatch (voiced)
    t  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "tT") <++> doesNotMatch (h) <++> doesNotMatch (glide) <++> doesNotMatch (t) <++> doesNotMatch (voiced)
    h  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "'h") <++> matches (nucleus)
    digit  <- newRule $ concatMany (comma) <++> (tolist1 <$> oneOf "0123456789") <++> doesNotMatch (h) <++> doesNotMatch (nucleus)
    post_word  <- newRule $ pause // doesNotMatch (nucleus) <++> lojban_word
    pause  <- newRule $ concatMany (comma) <++> concatSome (space_char)
    comma  <- newRule $ (tolist1 <$> oneOf ",")
    non_lojban_word  <- newRule $ doesNotMatch (lojban_word) <++> concatSome (non_space)
    non_space  <- newRule $ doesNotMatch (space_char)
    space_char  <- newRule $ (tolist1 <$> oneOf ".\t\n\r?! ")
    spaces  <- newRule $ doesNotMatch (class_Y) <++> concatSome (space_char)
    ybu  <- newRule $ class_Y <++> concatMany (space_char) <++> class_BU
    lujvo  <- newRule $ doesNotMatch (gismu) <++> doesNotMatch (fuhivla) <++> brivla
    class_A  <- newRule $ matches (cmavo) <++> (a // e // j <++> i // o // u) <++> matches (post_word)
    class_BAI  <- newRule $ matches (cmavo) <++> (d <++> u <++> h <++> o // s <++> i <++> h <++> u // z <++> a <++> u // k <++> i <++> h <++> i // d <++> u <++> h <++> i // c <++> u <++> h <++> u // t <++> u <++> h <++> i // t <++> i <++> h <++> u // d <++> i <++> h <++> o // j <++> i <++> h <++> u // r <++> i <++> h <++> a // n <++> i <++> h <++> i // m <++> u <++> h <++> i // k <++> i <++> h <++> u // v <++> a <++> h <++> u // k <++> o <++> i // c <++> a <++> h <++> i // t <++> a <++> h <++> i // p <++> u <++> h <++> e // j <++> a <++> h <++> i // k <++> a <++> i // b <++> a <++> i // f <++> i <++> h <++> e // d <++> e <++> h <++> i // c <++> i <++> h <++> o // m <++> a <++> u // m <++> u <++> h <++> u // r <++> i <++> h <++> i // r <++> a <++> h <++> i // k <++> a <++> h <++> a // p <++> a <++> h <++> u // p <++> a <++> h <++> a // l <++> e <++> h <++> a // k <++> u <++> h <++> u // t <++> a <++> i // b <++> a <++> u // m <++> a <++> h <++> i // c <++> i <++> h <++> e // f <++> a <++> u // p <++> o <++> h <++> i // c <++> a <++> u // m <++> a <++> h <++> e // c <++> i <++> h <++> u // r <++> a <++> h <++> a // p <++> u <++> h <++> a // l <++> i <++> h <++> e // l <++> a <++> h <++> u // b <++> a <++> h <++> i // k <++> a <++> h <++> i // s <++> a <++> u // f <++> a <++> h <++> e // b <++> e <++> h <++> i // t <++> i <++> h <++> i // j <++> a <++> h <++> e // g <++> a <++> h <++> a // v <++> a <++> h <++> o // j <++> i <++> h <++> o // m <++> e <++> h <++> a // d <++> o <++> h <++> e // j <++> i <++> h <++> e // p <++> i <++> h <++> o // g <++> a <++> u // z <++> u <++> h <++> e // m <++> e <++> h <++> e // r <++> a <++> i) <++> matches (post_word)
    class_BAhE  <- newRule $ matches (cmavo) <++> (b <++> a <++> h <++> e // z <++> a <++> h <++> e) <++> matches (post_word)
    class_BE  <- newRule $ matches (cmavo) <++> (b <++> e) <++> matches (post_word)
    class_BEI  <- newRule $ matches (cmavo) <++> (b <++> e <++> i) <++> matches (post_word)
    class_BEhO  <- newRule $ matches (cmavo) <++> (b <++> e <++> h <++> o) <++> matches (post_word)
    class_BIhE  <- newRule $ matches (cmavo) <++> (b <++> i <++> h <++> e) <++> matches (post_word)
    class_BIhI  <- newRule $ matches (cmavo) <++> (m <++> i <++> h <++> i // b <++> i <++> h <++> o // b <++> i <++> h <++> i) <++> matches (post_word)
    class_BO  <- newRule $ matches (cmavo) <++> (b <++> o) <++> matches (post_word)
    class_BOI  <- newRule $ matches (cmavo) <++> (b <++> o <++> i) <++> matches (post_word)
    class_BU  <- newRule $ matches (cmavo) <++> (b <++> u) <++> matches (post_word)
    class_BY  <- newRule $ matches (cmavo) <++> (ybu // j <++> o <++> h <++> o // r <++> u <++> h <++> o // g <++> e <++> h <++> o // j <++> e <++> h <++> o // l <++> o <++> h <++> a // n <++> a <++> h <++> a // s <++> e <++> h <++> e // t <++> o <++> h <++> a // g <++> a <++> h <++> e // y <++> h <++> y // b <++> y // c <++> y // d <++> y // f <++> y // g <++> y // j <++> y // k <++> y // l <++> y // m <++> y // n <++> y // p <++> y // r <++> y // s <++> y // t <++> y // v <++> y // x <++> y // z <++> y) <++> matches (post_word)
    class_CAhA  <- newRule $ matches (cmavo) <++> (c <++> a <++> h <++> a // p <++> u <++> h <++> i // n <++> u <++> h <++> o // k <++> a <++> h <++> e) <++> matches (post_word)
    class_CAI  <- newRule $ matches (cmavo) <++> (p <++> e <++> i // c <++> a <++> i // c <++> u <++> h <++> i // s <++> a <++> i // r <++> u <++> h <++> e) <++> matches (post_word)
    class_CEI  <- newRule $ matches (cmavo) <++> (c <++> e <++> i) <++> matches (post_word)
    class_CEhE  <- newRule $ matches (cmavo) <++> (c <++> e <++> h <++> e) <++> matches (post_word)
    class_CO  <- newRule $ matches (cmavo) <++> (c <++> o) <++> matches (post_word)
    class_COI  <- newRule $ matches (cmavo) <++> (j <++> u <++> h <++> i // c <++> o <++> i // f <++> i <++> h <++> i // t <++> a <++> h <++> a // m <++> u <++> h <++> o // f <++> e <++> h <++> o // c <++> o <++> h <++> o // p <++> e <++> h <++> u // k <++> e <++> h <++> o // n <++> u <++> h <++> e // r <++> e <++> h <++> i // b <++> e <++> h <++> e // j <++> e <++> h <++> e // m <++> i <++> h <++> e // k <++> i <++> h <++> e // v <++> i <++> h <++> o) <++> matches (post_word)
    class_CU  <- newRule $ matches (cmavo) <++> (c <++> u) <++> matches (post_word)
    class_CUhE  <- newRule $ matches (cmavo) <++> (c <++> u <++> h <++> e // n <++> a <++> u) <++> matches (post_word)
    class_DAhO  <- newRule $ matches (cmavo) <++> (d <++> a <++> h <++> o) <++> matches (post_word)
    class_DOI  <- newRule $ matches (cmavo) <++> (d <++> o <++> i) <++> matches (post_word)
    class_DOhU  <- newRule $ matches (cmavo) <++> (d <++> o <++> h <++> u) <++> matches (post_word)
    class_FA  <- newRule $ matches (cmavo) <++> (f <++> a <++> i // f <++> a // f <++> e // f <++> o // f <++> u // f <++> i <++> h <++> a // f <++> i) <++> matches (post_word)
    class_FAhA  <- newRule $ matches (cmavo) <++> (d <++> u <++> h <++> a // b <++> e <++> h <++> a // n <++> e <++> h <++> u // v <++> u <++> h <++> a // g <++> a <++> h <++> u // t <++> i <++> h <++> a // n <++> i <++> h <++> a // c <++> a <++> h <++> u // z <++> u <++> h <++> a // r <++> i <++> h <++> u // r <++> u <++> h <++> u // r <++> e <++> h <++> o // t <++> e <++> h <++> e // b <++> u <++> h <++> u // n <++> e <++> h <++> a // p <++> a <++> h <++> o // n <++> e <++> h <++> i // t <++> o <++> h <++> o // z <++> o <++> h <++> i // z <++> e <++> h <++> o // z <++> o <++> h <++> a // f <++> a <++> h <++> a) <++> matches (post_word) <++> matches (post_word)
    class_FAhO  <- newRule $ matches (cmavo) <++> (f <++> a <++> h <++> o) <++> matches (post_word)
    class_FEhE  <- newRule $ matches (cmavo) <++> (f <++> e <++> h <++> e) <++> matches (post_word)
    class_FEhU  <- newRule $ matches (cmavo) <++> (f <++> e <++> h <++> u) <++> matches (post_word)
    class_FIhO  <- newRule $ matches (cmavo) <++> (f <++> i <++> h <++> o) <++> matches (post_word)
    class_FOI  <- newRule $ matches (cmavo) <++> (f <++> o <++> i) <++> matches (post_word)
    class_FUhA  <- newRule $ matches (cmavo) <++> (f <++> u <++> h <++> a) <++> matches (post_word)
    class_FUhE  <- newRule $ matches (cmavo) <++> (f <++> u <++> h <++> e) <++> matches (post_word)
    class_FUhO  <- newRule $ matches (cmavo) <++> (f <++> u <++> h <++> o) <++> matches (post_word)
    class_GA  <- newRule $ matches (cmavo) <++> (g <++> e <++> h <++> i // g <++> e // g <++> o // g <++> a // g <++> u) <++> matches (post_word)
    class_GAhO  <- newRule $ matches (cmavo) <++> (k <++> e <++> h <++> i // g <++> a <++> h <++> o) <++> matches (post_word)
    class_GEhU  <- newRule $ matches (cmavo) <++> (g <++> e <++> h <++> u) <++> matches (post_word)
    class_GI  <- newRule $ matches (cmavo) <++> (g <++> i) <++> matches (post_word)
    class_GIhA  <- newRule $ matches (cmavo) <++> (g <++> i <++> h <++> e // g <++> i <++> h <++> i // g <++> i <++> h <++> o // g <++> i <++> h <++> a // g <++> i <++> h <++> u) <++> matches (post_word)
    class_GOI  <- newRule $ matches (cmavo) <++> (n <++> o <++> h <++> u // n <++> e // g <++> o <++> i // p <++> o <++> h <++> u // p <++> e // p <++> o <++> h <++> e // p <++> o) <++> matches (post_word)
    class_GOhA  <- newRule $ matches (cmavo) <++> (m <++> o // n <++> e <++> i // g <++> o <++> h <++> u // g <++> o <++> h <++> o // g <++> o <++> h <++> i // n <++> o <++> h <++> a // g <++> o <++> h <++> e // g <++> o <++> h <++> a // d <++> u // b <++> u <++> h <++> a // b <++> u <++> h <++> e // b <++> u <++> h <++> i // c <++> o <++> h <++> e) <++> matches (post_word)
    class_GUhA  <- newRule $ matches (cmavo) <++> (g <++> u <++> h <++> e // g <++> u <++> h <++> i // g <++> u <++> h <++> o // g <++> u <++> h <++> a // g <++> u <++> h <++> u) <++> matches (post_word)
    class_I  <- newRule $ matches (cmavo) <++> (i) <++> matches (post_word)
    class_JA  <- newRule $ matches (cmavo) <++> (j <++> e <++> h <++> i // j <++> e // j <++> o // j <++> a // j <++> u) <++> matches (post_word)
    class_JAI  <- newRule $ matches (cmavo) <++> (j <++> a <++> i) <++> matches (post_word)
    class_JOhI  <- newRule $ matches (cmavo) <++> (j <++> o <++> h <++> i) <++> matches (post_word)
    class_JOI  <- newRule $ matches (cmavo) <++> (f <++> a <++> h <++> u // p <++> i <++> h <++> u // j <++> o <++> i // c <++> e <++> h <++> o // c <++> e // j <++> o <++> h <++> u // k <++> u <++> h <++> a // j <++> o <++> h <++> e // j <++> u <++> h <++> e) <++> matches (post_word)
    class_KE  <- newRule $ matches (cmavo) <++> (k <++> e) <++> matches (post_word)
    class_KEhE  <- newRule $ matches (cmavo) <++> (k <++> e <++> h <++> e) <++> matches (post_word)
    class_KEI  <- newRule $ matches (cmavo) <++> (k <++> e <++> i) <++> matches (post_word)
    class_KI  <- newRule $ matches (cmavo) <++> (k <++> i) <++> matches (post_word)
    class_KOhA  <- newRule $ matches (cmavo) <++> (d <++> a <++> h <++> u // d <++> a <++> h <++> e // d <++> i <++> h <++> u // d <++> i <++> h <++> e // d <++> e <++> h <++> u // d <++> e <++> h <++> e // d <++> e <++> i // d <++> o <++> h <++> i // m <++> i <++> h <++> o // m <++> a <++> h <++> a // m <++> i <++> h <++> a // d <++> o <++> h <++> o // k <++> o <++> h <++> a // f <++> o <++> h <++> u // k <++> o <++> h <++> e // k <++> o <++> h <++> i // k <++> o <++> h <++> o // k <++> o <++> h <++> u // f <++> o <++> h <++> a // f <++> o <++> h <++> e // f <++> o <++> h <++> i // f <++> o <++> h <++> o // v <++> o <++> h <++> a // v <++> o <++> h <++> e // v <++> o <++> h <++> i // v <++> o <++> h <++> o // v <++> o <++> h <++> u // r <++> u // r <++> i // r <++> a // t <++> a // t <++> u // t <++> i // z <++> i <++> h <++> o // k <++> e <++> h <++> a // m <++> a // z <++> u <++> h <++> i // z <++> o <++> h <++> e // c <++> e <++> h <++> u // d <++> a // d <++> e // d <++> i // k <++> o // m <++> i // d <++> o) <++> matches (post_word)
    class_KU  <- newRule $ matches (cmavo) <++> (k <++> u) <++> matches (post_word)
    class_KUhE  <- newRule $ matches (cmavo) <++> (k <++> u <++> h <++> e) <++> matches (post_word)
    class_KUhO  <- newRule $ matches (cmavo) <++> (k <++> u <++> h <++> o) <++> matches (post_word)
    class_LA  <- newRule $ matches (cmavo) <++> (l <++> a <++> i // l <++> a <++> h <++> i // l <++> a) <++> matches (post_word)
    class_LAU  <- newRule $ matches (cmavo) <++> (c <++> e <++> h <++> a // l <++> a <++> u // z <++> a <++> i // t <++> a <++> u) <++> matches (post_word)
    class_LAhE  <- newRule $ matches (cmavo) <++> (t <++> u <++> h <++> a // l <++> u <++> h <++> a // l <++> u <++> h <++> o // l <++> a <++> h <++> e // v <++> u <++> h <++> i // l <++> u <++> h <++> i // l <++> u <++> h <++> e) <++> matches (post_word)
    class_LE  <- newRule $ matches (cmavo) <++> (l <++> e <++> i // l <++> o <++> i // l <++> e <++> h <++> i // l <++> o <++> h <++> i // l <++> e <++> h <++> e // l <++> o <++> h <++> e // l <++> o // l <++> e) <++> matches (post_word)
    class_LEhU  <- newRule $ matches (cmavo) <++> (l <++> e <++> h <++> u) <++> matches (post_word)
    class_LI  <- newRule $ matches (cmavo) <++> (m <++> e <++> h <++> o // l <++> i) <++> matches (post_word)
    class_LIhU  <- newRule $ matches (cmavo) <++> (l <++> i <++> h <++> u) <++> matches (post_word)
    class_LOhO  <- newRule $ matches (cmavo) <++> (l <++> o <++> h <++> o) <++> matches (post_word)
    class_LOhU  <- newRule $ matches (cmavo) <++> (l <++> o <++> h <++> u) <++> matches (post_word)
    class_LU  <- newRule $ matches (cmavo) <++> (l <++> u) <++> matches (post_word)
    class_LUhU  <- newRule $ matches (cmavo) <++> (l <++> u <++> h <++> u) <++> matches (post_word)
    class_MAhO  <- newRule $ matches (cmavo) <++> (m <++> a <++> h <++> o) <++> matches (post_word)
    class_MAI  <- newRule $ matches (cmavo) <++> (m <++> o <++> h <++> o // m <++> a <++> i) <++> matches (post_word)
    class_ME  <- newRule $ matches (cmavo) <++> (m <++> e) <++> matches (post_word)
    class_MEhU  <- newRule $ matches (cmavo) <++> (m <++> e <++> h <++> u) <++> matches (post_word)
    class_MOhE  <- newRule $ matches (cmavo) <++> (m <++> o <++> h <++> e) <++> matches (post_word)
    class_MOhI  <- newRule $ matches (cmavo) <++> (m <++> o <++> h <++> i) <++> matches (post_word)
    class_MOI  <- newRule $ matches (cmavo) <++> (m <++> e <++> i // m <++> o <++> i // s <++> i <++> h <++> e // c <++> u <++> h <++> o // v <++> a <++> h <++> e) <++> matches (post_word)
    class_NA  <- newRule $ matches (cmavo) <++> (j <++> a <++> h <++> a // n <++> a) <++> matches (post_word)
    class_NAI  <- newRule $ matches (cmavo) <++> (n <++> a <++> i) <++> matches (post_word)
    class_NAhE  <- newRule $ matches (cmavo) <++> (t <++> o <++> h <++> e // j <++> e <++> h <++> a // n <++> a <++> h <++> e // n <++> o <++> h <++> e) <++> matches (post_word)
    class_NAhU  <- newRule $ matches (cmavo) <++> (n <++> a <++> h <++> u) <++> matches (post_word)
    class_NIhE  <- newRule $ matches (cmavo) <++> (n <++> i <++> h <++> e) <++> matches (post_word)
    class_NIhO  <- newRule $ matches (cmavo) <++> (n <++> i <++> h <++> o // n <++> o <++> h <++> i) <++> matches (post_word)
    class_NOI  <- newRule $ matches (cmavo) <++> (v <++> o <++> i // n <++> o <++> i // p <++> o <++> i) <++> matches (post_word)
    class_NU  <- newRule $ matches (cmavo) <++> (n <++> i // d <++> u <++> h <++> u // s <++> i <++> h <++> o // n <++> u // l <++> i <++> h <++> i // k <++> a // j <++> e <++> i // s <++> u <++> h <++> u // z <++> u <++> h <++> o // m <++> u <++> h <++> e // p <++> u <++> h <++> u // z <++> a <++> h <++> i) <++> matches (post_word)
    class_NUhA  <- newRule $ matches (cmavo) <++> (n <++> u <++> h <++> a) <++> matches (post_word)
    class_NUhI  <- newRule $ matches (cmavo) <++> (n <++> u <++> h <++> i) <++> matches (post_word)
    class_NUhU  <- newRule $ matches (cmavo) <++> (n <++> u <++> h <++> u) <++> matches (post_word)
    class_PA  <- newRule $ matches (cmavo) <++> (d <++> a <++> u // f <++> e <++> i // g <++> a <++> i // j <++> a <++> u // r <++> e <++> i // v <++> a <++> i // p <++> i <++> h <++> e // p <++> i // f <++> i <++> h <++> u // z <++> a <++> h <++> u // m <++> e <++> h <++> i // n <++> i <++> h <++> u // k <++> i <++> h <++> o // c <++> e <++> h <++> i // m <++> a <++> h <++> u // r <++> a <++> h <++> e // d <++> a <++> h <++> a // s <++> o <++> h <++> a // j <++> i <++> h <++> i // s <++> u <++> h <++> o // s <++> u <++> h <++> e // r <++> o // r <++> a <++> u // s <++> o <++> h <++> u // s <++> o <++> h <++> i // s <++> o <++> h <++> e // s <++> o <++> h <++> o // m <++> o <++> h <++> a // d <++> u <++> h <++> e // t <++> e <++> h <++> o // k <++> a <++> h <++> o // c <++> i <++> h <++> i // t <++> u <++> h <++> o // x <++> o // p <++> a <++> i // n <++> o <++> h <++> o // n <++> o // p <++> a // r <++> e // c <++> i // v <++> o // m <++> u // x <++> a // z <++> e // b <++> i // s <++> o // digit) <++> matches (post_word)
    class_PEhE  <- newRule $ matches (cmavo) <++> (p <++> e <++> h <++> e) <++> matches (post_word)
    class_PEhO  <- newRule $ matches (cmavo) <++> (p <++> e <++> h <++> o) <++> matches (post_word)
    class_PU  <- newRule $ matches (cmavo) <++> (b <++> a // p <++> u // c <++> a) <++> matches (post_word)
    class_RAhO  <- newRule $ matches (cmavo) <++> (r <++> a <++> h <++> o) <++> matches (post_word)
    class_ROI  <- newRule $ matches (cmavo) <++> (r <++> e <++> h <++> u // r <++> o <++> i) <++> matches (post_word)
    class_SA  <- newRule $ matches (cmavo) <++> (s <++> a) <++> matches (post_word)
    class_SE  <- newRule $ matches (cmavo) <++> (s <++> e // t <++> e // v <++> e // x <++> e) <++> matches (post_word)
    class_SEI  <- newRule $ matches (cmavo) <++> (s <++> e <++> i // t <++> i <++> h <++> o) <++> matches (post_word)
    class_SEhU  <- newRule $ matches (cmavo) <++> (s <++> e <++> h <++> u) <++> matches (post_word)
    class_SI  <- newRule $ matches (cmavo) <++> (s <++> i) <++> matches (post_word)
    class_SOI  <- newRule $ matches (cmavo) <++> (s <++> o <++> i) <++> matches (post_word)
    class_SU  <- newRule $ matches (cmavo) <++> (s <++> u) <++> matches (post_word)
    class_TAhE  <- newRule $ matches (cmavo) <++> (r <++> u <++> h <++> i // t <++> a <++> h <++> e // d <++> i <++> h <++> i // n <++> a <++> h <++> o) <++> matches (post_word)
    class_TEhU  <- newRule $ matches (cmavo) <++> (t <++> e <++> h <++> u) <++> matches (post_word)
    class_TEI  <- newRule $ matches (cmavo) <++> (t <++> e <++> i) <++> matches (post_word)
    class_TO  <- newRule $ matches (cmavo) <++> (t <++> o <++> h <++> i // t <++> o) <++> matches (post_word)
    class_TOI  <- newRule $ matches (cmavo) <++> (t <++> o <++> i) <++> matches (post_word)
    class_TUhE  <- newRule $ matches (cmavo) <++> (t <++> u <++> h <++> e) <++> matches (post_word)
    class_TUhU  <- newRule $ matches (cmavo) <++> (t <++> u <++> h <++> u) <++> matches (post_word)
    class_UI  <- newRule $ matches (cmavo) <++> (i <++> h <++> a // i <++> e // a <++> h <++> e // u <++> h <++> i // i <++> h <++> o // i <++> h <++> e // a <++> h <++> a // i <++> a // o <++> h <++> i // o <++> h <++> e // e <++> h <++> e // o <++> i // u <++> o // e <++> h <++> i // u <++> h <++> o // a <++> u // u <++> a // a <++> h <++> i // i <++> h <++> u // i <++> i // u <++> h <++> a // u <++> i // a <++> h <++> o // a <++> i // a <++> h <++> u // i <++> u // e <++> i // o <++> h <++> o // e <++> h <++> a // u <++> u // o <++> h <++> a // o <++> h <++> u // u <++> h <++> u // e <++> h <++> o // i <++> o // e <++> h <++> u // u <++> e // i <++> h <++> i // u <++> h <++> e // b <++> a <++> h <++> a // j <++> a <++> h <++> o // c <++> a <++> h <++> e // s <++> u <++> h <++> a // t <++> i <++> h <++> e // k <++> a <++> h <++> u // s <++> e <++> h <++> o // z <++> a <++> h <++> a // p <++> e <++> h <++> i // r <++> u <++> h <++> a // j <++> u <++> h <++> a // t <++> a <++> h <++> o // r <++> a <++> h <++> u // l <++> i <++> h <++> a // b <++> a <++> h <++> u // m <++> u <++> h <++> a // d <++> o <++> h <++> a // t <++> o <++> h <++> u // v <++> a <++> h <++> i // p <++> a <++> h <++> e // z <++> u <++> h <++> u // s <++> a <++> h <++> e // l <++> a <++> h <++> a // k <++> e <++> h <++> u // s <++> a <++> h <++> u // d <++> a <++> h <++> i // j <++> e <++> h <++> u // s <++> a <++> h <++> a // k <++> a <++> u // t <++> a <++> h <++> u // n <++> a <++> h <++> i // j <++> o <++> h <++> a // b <++> i <++> h <++> u // l <++> i <++> h <++> o // p <++> a <++> u // m <++> i <++> h <++> u // k <++> u <++> h <++> i // j <++> i <++> h <++> a // s <++> i <++> h <++> a // p <++> o <++> h <++> o // p <++> e <++> h <++> a // r <++> o <++> h <++> i // r <++> o <++> h <++> e // r <++> o <++> h <++> o // r <++> o <++> h <++> u // r <++> o <++> h <++> a // r <++> e <++> h <++> e // l <++> e <++> h <++> o // j <++> u <++> h <++> o // f <++> u <++> h <++> i // d <++> a <++> i // g <++> a <++> h <++> i // z <++> o <++> h <++> o // b <++> e <++> h <++> u // r <++> i <++> h <++> e // s <++> e <++> h <++> i // s <++> e <++> h <++> a // v <++> u <++> h <++> e // k <++> i <++> h <++> a // x <++> u // g <++> e <++> h <++> e // b <++> u <++> h <++> o) <++> matches (post_word)
    class_VA  <- newRule $ matches (cmavo) <++> (v <++> i // v <++> a // v <++> u) <++> matches (post_word)
    class_VAU  <- newRule $ matches (cmavo) <++> (v <++> a <++> u) <++> matches (post_word)
    class_VEI  <- newRule $ matches (cmavo) <++> (v <++> e <++> i) <++> matches (post_word)
    class_VEhO  <- newRule $ matches (cmavo) <++> (v <++> e <++> h <++> o) <++> matches (post_word)
    class_VUhU  <- newRule $ matches (cmavo) <++> (g <++> e <++> h <++> a // f <++> u <++> h <++> u // p <++> i <++> h <++> i // f <++> e <++> h <++> i // v <++> u <++> h <++> u // s <++> u <++> h <++> i // j <++> u <++> h <++> u // g <++> e <++> i // p <++> a <++> h <++> i // f <++> a <++> h <++> i // t <++> e <++> h <++> a // c <++> u <++> h <++> a // v <++> a <++> h <++> a // n <++> e <++> h <++> o // d <++> e <++> h <++> o // f <++> e <++> h <++> a // s <++> a <++> h <++> o // r <++> e <++> h <++> a // r <++> i <++> h <++> o // s <++> a <++> h <++> i // p <++> i <++> h <++> a // s <++> i <++> h <++> i) <++> matches (post_word)
    class_VEhA  <- newRule $ matches (cmavo) <++> (v <++> e <++> h <++> u // v <++> e <++> h <++> a // v <++> e <++> h <++> i // v <++> e <++> h <++> e) <++> matches (post_word)
    class_VIhA  <- newRule $ matches (cmavo) <++> (v <++> i <++> h <++> i // v <++> i <++> h <++> a // v <++> i <++> h <++> u // v <++> i <++> h <++> e) <++> matches (post_word)
    class_VUhO  <- newRule $ matches (cmavo) <++> (v <++> u <++> h <++> o) <++> matches (post_word)
    class_XI  <- newRule $ matches (cmavo) <++> (x <++> i) <++> matches (post_word)
    class_Y  <- newRule $ matches (cmavo) <++> (concatSome (y)) <++> matches (post_word)
    class_ZAhO  <- newRule $ matches (cmavo) <++> (c <++> o <++> h <++> i // p <++> u <++> h <++> o // c <++> o <++> h <++> u // m <++> o <++> h <++> u // c <++> a <++> h <++> o // c <++> o <++> h <++> a // d <++> e <++> h <++> a // b <++> a <++> h <++> o // d <++> i <++> h <++> a // z <++> a <++> h <++> o) <++> matches (post_word)
    class_ZEhA  <- newRule $ matches (cmavo) <++> (z <++> e <++> h <++> u // z <++> e <++> h <++> a // z <++> e <++> h <++> i // z <++> e <++> h <++> e) <++> matches (post_word)
    class_ZEI  <- newRule $ matches (cmavo) <++> (z <++> e <++> i) <++> matches (post_word)
    class_ZI  <- newRule $ matches (cmavo) <++> (z <++> u // z <++> a // z <++> i) <++> matches (post_word)
    class_ZIhE  <- newRule $ matches (cmavo) <++> (z <++> i <++> h <++> e) <++> matches (post_word)
    class_ZO  <- newRule $ matches (cmavo) <++> (z <++> o) <++> matches (post_word)
    class_ZOI  <- newRule $ matches (cmavo) <++> (z <++> o <++> i // l <++> a <++> h <++> o) <++> matches (post_word)
    class_ZOhU  <- newRule $ matches (cmavo) <++> (z <++> o <++> h <++> u) <++> matches (post_word)
    return $ case _class of
                "brivla" -> (Just <$> class_BRIVLA) // unit Nothing
                "cmevla" -> (Just <$> class_CMEVLA) // unit Nothing
                "by" -> (Just <$> class_BY) // unit Nothing
