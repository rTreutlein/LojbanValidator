module Lojban.Syntax.Morph where

import Prelude hiding (id,(.),(<*>),(<$>),(*>),(<*),foldl)

import Control.Category
import Control.Arrow hiding (left,right)
import Control.Applicative hiding (many,some,optional)
import Control.Monad
import Control.Monad.RWS.Class
import Control.Monad.Trans.Class


import Iso
import Syntax hiding (SynIso,Syntax)
import Lojban.Syntax.Types
import Lojban.Syntax.Util hiding (any_word,digit,gismu)

class_CMEVLA  :: SyntaxState s => Syntax s String
class_CMEVLA  = cmevla

class_BRIVLA  :: SyntaxState s => Syntax s String
class_BRIVLA  = gismu <+> lujvo <+> fuhivla

class_CMAVO  :: SyntaxState s => Syntax s String
class_CMAVO  = class_A <+> class_BAI <+> class_BAhE <+> class_BE <+> class_BEI <+> class_BEhO <+> class_BIhE <+> class_BIhI <+> class_BO <+> class_BOI <+> class_BU <+> class_BY <+> class_CAhA <+> class_CAI <+> class_CEI <+> class_CEhE <+> class_CO <+> class_COI <+> class_CU <+> class_CUhE <+> class_DAhO <+> class_DOI <+> class_DOhU <+> class_FA <+> class_FAhA <+> class_FAhO <+> class_FEhE <+> class_FEhU <+> class_FIhO <+> class_FOI <+> class_FUhA <+> class_FUhE <+> class_FUhO <+> class_GA <+> class_GAhO <+> class_GEhU <+> class_GI <+> class_GIhA <+> class_GOI <+> class_GOhA <+> class_GUhA <+> class_I <+> class_JA <+> class_JAI <+> class_JOhI <+> class_JOI <+> class_KE <+> class_KEhE <+> class_KEI <+> class_KI <+> class_KOhA <+> class_KU <+> class_KUhE <+> class_KUhO <+> class_LA <+> class_LAU <+> class_LAhE <+> class_LE <+> class_LEhU <+> class_LI <+> class_LIhU <+> class_LOhO <+> class_LOhU <+> class_LU <+> class_LUhU <+> class_MAhO <+> class_MAI <+> class_ME <+> class_MEhU <+> class_MOhE <+> class_MOhI <+> class_MOI <+> class_NA <+> class_NAI <+> class_NAhE <+> class_NAhU <+> class_NIhE <+> class_NIhO <+> class_NOI <+> class_NU <+> class_NUhA <+> class_NUhI <+> class_NUhU <+> class_PA <+> class_PEhE <+> class_PEhO <+> class_PU <+> class_RAhO <+> class_ROI <+> class_SA <+> class_SE <+> class_SEI <+> class_SEhU <+> class_SI <+> class_SOI <+> class_SU <+> class_TAhE <+> class_TEhU <+> class_TEI <+> class_TO <+> class_TOI <+> class_TUhE <+> class_TUhU <+> class_UI <+> class_VA <+> class_VAU <+> class_VEI <+> class_VEhO <+> class_VUhU <+> class_VEhA <+> class_VIhA <+> class_VUhO <+> class_XI <+> class_ZAhO <+> class_ZEhA <+> class_ZEI <+> class_ZI <+> class_ZIhE <+> class_ZO <+> class_ZOI <+> class_ZOhU <+> cmavo

lojban_word  :: SyntaxState s => Syntax s String
lojban_word  = class_CMEVLA <+> class_CMAVO <+> class_BRIVLA

any_word  :: SyntaxState s => Syntax s String
any_word  = lojban_word &+& listoptional (spaces)

zoi_open  :: SyntaxState s => Syntax s String
zoi_open  = lojban_word

zoi_word  :: SyntaxState s => Syntax s String
zoi_word  = concatSome (non_space)

zoi_close  :: SyntaxState s => Syntax s String
zoi_close  = any_word

cmevla  :: SyntaxState s => Syntax s String
cmevla  = jbocme <+> zifcme

zifcme  :: SyntaxState s => Syntax s String
zifcme  = notsyn (h) &+& concatMany ((nucleus <+> glide <+> h <+> consonant &+& notsyn (pause) <+> digit)) &+& consonant &+& syn (pause)

jbocme  :: SyntaxState s => Syntax s String
jbocme  = syn (zifcme) &+& concatMany ((any_syllable <+> digit)) &+& syn (pause)

cmavo  :: SyntaxState s => Syntax s String
cmavo  = notsyn (cmevla) &+& notsyn (class_CVCy_lujvo) &+& cmavo_form &+& syn (post_word)

class_CVCy_lujvo  :: SyntaxState s => Syntax s String
class_CVCy_lujvo  = class_CVC_rafsi &+& y &+& listoptional (h) &+& concatMany (initial_rafsi) &+& brivla_core <+> stressed_CVC_rafsi &+& y &+& short_final_rafsi

cmavo_form  :: SyntaxState s => Syntax s String
cmavo_form  = notsyn (h) &+& notsyn (cluster) &+& onset &+& concatMany ((nucleus &+& h)) &+& (notsyn (stressed) &+& nucleus <+> nucleus &+& notsyn (cluster)) <+> concatSome (y) <+> digit

brivla  :: SyntaxState s => Syntax s String
brivla  = notsyn (cmavo) &+& concatMany (initial_rafsi) &+& brivla_core

brivla_core  :: SyntaxState s => Syntax s String
brivla_core  = fuhivla <+> gismu <+> class_CVV_final_rafsi <+> stressed_initial_rafsi &+& short_final_rafsi

stressed_initial_rafsi  :: SyntaxState s => Syntax s String
stressed_initial_rafsi  = stressed_extended_rafsi <+> stressed_y_rafsi <+> stressed_y_less_rafsi

initial_rafsi  :: SyntaxState s => Syntax s String
initial_rafsi  = extended_rafsi <+> y_rafsi <+> notsyn (any_extended_rafsi) &+& y_less_rafsi &+& notsyn (any_extended_rafsi)

any_extended_rafsi  :: SyntaxState s => Syntax s String
any_extended_rafsi  = fuhivla <+> extended_rafsi <+> stressed_extended_rafsi

fuhivla  :: SyntaxState s => Syntax s String
fuhivla  = fuhivla_head &+& stressed_syllable &+& concatMany (consonantal_syllable) &+& final_syllable

stressed_extended_rafsi  :: SyntaxState s => Syntax s String
stressed_extended_rafsi  = stressed_brivla_rafsi <+> stressed_fuhivla_rafsi

extended_rafsi  :: SyntaxState s => Syntax s String
extended_rafsi  = brivla_rafsi <+> fuhivla_rafsi

stressed_brivla_rafsi  :: SyntaxState s => Syntax s String
stressed_brivla_rafsi  = syn (unstressed_syllable) &+& brivla_head &+& stressed_syllable &+& h &+& y

brivla_rafsi  :: SyntaxState s => Syntax s String
brivla_rafsi  = syn ((syllable &+& concatMany (consonantal_syllable) &+& syllable)) &+& brivla_head &+& h &+& y &+& listoptional (h)

stressed_fuhivla_rafsi  :: SyntaxState s => Syntax s String
stressed_fuhivla_rafsi  = fuhivla_head &+& stressed_syllable &+& concatMany (consonantal_syllable) &+& notsyn (h) &+& onset &+& y

fuhivla_rafsi  :: SyntaxState s => Syntax s String
fuhivla_rafsi  = syn (unstressed_syllable) &+& fuhivla_head &+& notsyn (h) &+& onset &+& y &+& listoptional (h)

fuhivla_head  :: SyntaxState s => Syntax s String
fuhivla_head  = notsyn (rafsi_string) &+& brivla_head

brivla_head  :: SyntaxState s => Syntax s String
brivla_head  = notsyn (cmavo) &+& notsyn (slinkuhi) &+& notsyn (h) &+& syn (onset) &+& concatMany (unstressed_syllable)

slinkuhi  :: SyntaxState s => Syntax s String
slinkuhi  = notsyn (rafsi_string) &+& consonant &+& rafsi_string

rafsi_string  :: SyntaxState s => Syntax s String
rafsi_string  = concatMany (y_less_rafsi) &+& (gismu <+> class_CVV_final_rafsi <+> stressed_y_less_rafsi &+& short_final_rafsi <+> y_rafsi <+> stressed_y_rafsi <+> listoptional (stressed_y_less_rafsi) &+& initial_pair &+& y <+> hy_rafsi <+> stressed_hy_rafsi)

gismu  :: SyntaxState s => Syntax s String
gismu  = (initial_pair &+& stressed_vowel <+> consonant &+& stressed_vowel &+& consonant) &+& syn (final_syllable) &+& consonant &+& vowel &+& syn (post_word)

class_CVV_final_rafsi  :: SyntaxState s => Syntax s String
class_CVV_final_rafsi  = consonant &+& stressed_vowel &+& h &+& syn (final_syllable) &+& vowel &+& syn (post_word)

short_final_rafsi  :: SyntaxState s => Syntax s String
short_final_rafsi  = syn (final_syllable) &+& (consonant &+& diphthong <+> initial_pair &+& vowel) &+& syn (post_word)

stressed_y_rafsi  :: SyntaxState s => Syntax s String
stressed_y_rafsi  = (stressed_long_rafsi <+> stressed_CVC_rafsi) &+& y

stressed_y_less_rafsi  :: SyntaxState s => Syntax s String
stressed_y_less_rafsi  = stressed_CVC_rafsi &+& notsyn (y) <+> stressed_CCV_rafsi <+> stressed_CVV_rafsi

stressed_long_rafsi  :: SyntaxState s => Syntax s String
stressed_long_rafsi  = initial_pair &+& stressed_vowel &+& consonant <+> consonant &+& stressed_vowel &+& consonant &+& consonant

stressed_CVC_rafsi  :: SyntaxState s => Syntax s String
stressed_CVC_rafsi  = consonant &+& stressed_vowel &+& consonant

stressed_CCV_rafsi  :: SyntaxState s => Syntax s String
stressed_CCV_rafsi  = initial_pair &+& stressed_vowel

stressed_CVV_rafsi  :: SyntaxState s => Syntax s String
stressed_CVV_rafsi  = consonant &+& (unstressed_vowel &+& h &+& stressed_vowel <+> stressed_diphthong) &+& listoptional (r_hyphen)

y_rafsi  :: SyntaxState s => Syntax s String
y_rafsi  = (long_rafsi <+> class_CVC_rafsi) &+& y &+& listoptional (h)

y_less_rafsi  :: SyntaxState s => Syntax s String
y_less_rafsi  = notsyn (y_rafsi) &+& notsyn (stressed_y_rafsi) &+& notsyn (hy_rafsi) &+& notsyn (stressed_hy_rafsi) &+& (class_CVC_rafsi <+> class_CCV_rafsi <+> class_CVV_rafsi) &+& notsyn (h)

hy_rafsi  :: SyntaxState s => Syntax s String
hy_rafsi  = (long_rafsi &+& vowel <+> class_CCV_rafsi <+> class_CVV_rafsi) &+& h &+& y &+& listoptional (h)

stressed_hy_rafsi  :: SyntaxState s => Syntax s String
stressed_hy_rafsi  = (long_rafsi &+& stressed_vowel <+> stressed_CCV_rafsi <+> stressed_CVV_rafsi) &+& h &+& y

long_rafsi  :: SyntaxState s => Syntax s String
long_rafsi  = initial_pair &+& unstressed_vowel &+& consonant <+> consonant &+& unstressed_vowel &+& consonant &+& consonant

class_CVC_rafsi  :: SyntaxState s => Syntax s String
class_CVC_rafsi  = consonant &+& unstressed_vowel &+& consonant

class_CCV_rafsi  :: SyntaxState s => Syntax s String
class_CCV_rafsi  = initial_pair &+& unstressed_vowel

class_CVV_rafsi  :: SyntaxState s => Syntax s String
class_CVV_rafsi  = consonant &+& (unstressed_vowel &+& h &+& unstressed_vowel <+> unstressed_diphthong) &+& listoptional (r_hyphen)

r_hyphen  :: SyntaxState s => Syntax s String
r_hyphen  = r &+& syn (consonant) <+> n &+& syn (r)

final_syllable  :: SyntaxState s => Syntax s String
final_syllable  = onset &+& notsyn (y) &+& notsyn (stressed) &+& nucleus &+& notsyn (cmevla) &+& syn (post_word)

stressed_syllable  :: SyntaxState s => Syntax s String
stressed_syllable  = syn (stressed) &+& syllable <+> syllable &+& syn (stress)

stressed_diphthong  :: SyntaxState s => Syntax s String
stressed_diphthong  = syn (stressed) &+& diphthong <+> diphthong &+& syn (stress)

stressed_vowel  :: SyntaxState s => Syntax s String
stressed_vowel  = syn (stressed) &+& vowel <+> vowel &+& syn (stress)

unstressed_syllable  :: SyntaxState s => Syntax s String
unstressed_syllable  = notsyn (stressed) &+& syllable &+& notsyn (stress) <+> consonantal_syllable

unstressed_diphthong  :: SyntaxState s => Syntax s String
unstressed_diphthong  = notsyn (stressed) &+& diphthong &+& notsyn (stress)

unstressed_vowel  :: SyntaxState s => Syntax s String
unstressed_vowel  = notsyn (stressed) &+& vowel &+& notsyn (stress)

stress  :: SyntaxState s => Syntax s String
stress  = concatMany ((consonant <+> glide)) &+& listoptional (h) &+& listoptional (y) &+& syllable &+& pause

stressed  :: SyntaxState s => Syntax s String
stressed  = onset &+& concatMany (comma) &+& (tolist1 . token (`elem` "AEIOU"))

any_syllable  :: SyntaxState s => Syntax s String
any_syllable  = onset &+& nucleus &+& listoptional (coda) <+> consonantal_syllable

syllable  :: SyntaxState s => Syntax s String
syllable  = onset &+& notsyn (y) &+& nucleus &+& listoptional (coda)

consonantal_syllable  :: SyntaxState s => Syntax s String
consonantal_syllable  = consonant &+& syn (syllabic) &+& coda

coda  :: SyntaxState s => Syntax s String
coda  = notsyn (any_syllable) &+& consonant &+& syn (any_syllable) <+> listoptional (syllabic) &+& listoptional (consonant) &+& syn (pause)

onset  :: SyntaxState s => Syntax s String
onset  = h <+> glide <+> initial

nucleus  :: SyntaxState s => Syntax s String
nucleus  = vowel <+> diphthong <+> y &+& notsyn (nucleus)

glide  :: SyntaxState s => Syntax s String
glide  = (i <+> u) &+& syn (nucleus)

diphthong  :: SyntaxState s => Syntax s String
diphthong  = (a &+& i &+& notsyn (i) <+> a &+& u &+& notsyn (u) <+> e &+& i &+& notsyn (i) <+> o &+& i &+& notsyn (i)) &+& notsyn (nucleus)

vowel  :: SyntaxState s => Syntax s String
vowel  = (a <+> e <+> i <+> o <+> u) &+& notsyn (nucleus)

a  :: SyntaxState s => Syntax s String
a  = concatMany (comma) &+& (tolist1 . token (`elem` "aA"))

e  :: SyntaxState s => Syntax s String
e  = concatMany (comma) &+& (tolist1 . token (`elem` "eE"))

i  :: SyntaxState s => Syntax s String
i  = concatMany (comma) &+& (tolist1 . token (`elem` "iI"))

o  :: SyntaxState s => Syntax s String
o  = concatMany (comma) &+& (tolist1 . token (`elem` "oO"))

u  :: SyntaxState s => Syntax s String
u  = concatMany (comma) &+& (tolist1 . token (`elem` "uU"))

y  :: SyntaxState s => Syntax s String
y  = concatMany (comma) &+& (tolist1 . token (`elem` "yY")) &+& notsyn ((notsyn (y) &+& nucleus))

cluster  :: SyntaxState s => Syntax s String
cluster  = consonant &+& concatSome (consonant)

initial_pair  :: SyntaxState s => Syntax s String
initial_pair  = syn (initial) &+& consonant &+& consonant &+& notsyn (consonant)

initial  :: SyntaxState s => Syntax s String
initial  = (affricate <+> listoptional (sibilant) &+& listoptional (other) &+& listoptional (liquid)) &+& notsyn (consonant) &+& notsyn (glide)

affricate  :: SyntaxState s => Syntax s String
affricate  = t &+& c <+> t &+& s <+> d &+& j <+> d &+& z

liquid  :: SyntaxState s => Syntax s String
liquid  = l <+> r

other  :: SyntaxState s => Syntax s String
other  = p <+> t &+& notsyn (l) <+> k <+> f <+> x <+> b <+> d &+& notsyn (l) <+> g <+> v <+> m <+> n &+& notsyn (liquid)

sibilant  :: SyntaxState s => Syntax s String
sibilant  = c <+> s &+& notsyn (x) <+> (j <+> z) &+& notsyn (n) &+& notsyn (liquid)

consonant  :: SyntaxState s => Syntax s String
consonant  = voiced <+> unvoiced <+> syllabic

syllabic  :: SyntaxState s => Syntax s String
syllabic  = l <+> m <+> n <+> r

voiced  :: SyntaxState s => Syntax s String
voiced  = b <+> d <+> g <+> j <+> v <+> z

unvoiced  :: SyntaxState s => Syntax s String
unvoiced  = c <+> f <+> k <+> p <+> s <+> t <+> x

l  :: SyntaxState s => Syntax s String
l  = concatMany (comma) &+& (tolist1 . token (`elem` "lL")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (l)

m  :: SyntaxState s => Syntax s String
m  = concatMany (comma) &+& (tolist1 . token (`elem` "mM")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (m) &+& notsyn (z)

n  :: SyntaxState s => Syntax s String
n  = concatMany (comma) &+& (tolist1 . token (`elem` "nN")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (n) &+& notsyn (affricate)

r  :: SyntaxState s => Syntax s String
r  = concatMany (comma) &+& (tolist1 . token (`elem` "rR")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (r)

b  :: SyntaxState s => Syntax s String
b  = concatMany (comma) &+& (tolist1 . token (`elem` "bB")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (b) &+& notsyn (unvoiced)

d  :: SyntaxState s => Syntax s String
d  = concatMany (comma) &+& (tolist1 . token (`elem` "dD")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (d) &+& notsyn (unvoiced)

g  :: SyntaxState s => Syntax s String
g  = concatMany (comma) &+& (tolist1 . token (`elem` "gG")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (g) &+& notsyn (unvoiced)

v  :: SyntaxState s => Syntax s String
v  = concatMany (comma) &+& (tolist1 . token (`elem` "vV")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (v) &+& notsyn (unvoiced)

j  :: SyntaxState s => Syntax s String
j  = concatMany (comma) &+& (tolist1 . token (`elem` "jJ")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (j) &+& notsyn (z) &+& notsyn (unvoiced)

z  :: SyntaxState s => Syntax s String
z  = concatMany (comma) &+& (tolist1 . token (`elem` "zZ")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (z) &+& notsyn (j) &+& notsyn (unvoiced)

s  :: SyntaxState s => Syntax s String
s  = concatMany (comma) &+& (tolist1 . token (`elem` "sS")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (s) &+& notsyn (c) &+& notsyn (voiced)

c  :: SyntaxState s => Syntax s String
c  = concatMany (comma) &+& (tolist1 . token (`elem` "cC")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (c) &+& notsyn (s) &+& notsyn (x) &+& notsyn (voiced)

x  :: SyntaxState s => Syntax s String
x  = concatMany (comma) &+& (tolist1 . token (`elem` "xX")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (x) &+& notsyn (c) &+& notsyn (k) &+& notsyn (voiced)

k  :: SyntaxState s => Syntax s String
k  = concatMany (comma) &+& (tolist1 . token (`elem` "kK")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (k) &+& notsyn (x) &+& notsyn (voiced)

f  :: SyntaxState s => Syntax s String
f  = concatMany (comma) &+& (tolist1 . token (`elem` "fF")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (f) &+& notsyn (voiced)

p  :: SyntaxState s => Syntax s String
p  = concatMany (comma) &+& (tolist1 . token (`elem` "pP")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (p) &+& notsyn (voiced)

t  :: SyntaxState s => Syntax s String
t  = concatMany (comma) &+& (tolist1 . token (`elem` "tT")) &+& notsyn (h) &+& notsyn (glide) &+& notsyn (t) &+& notsyn (voiced)

h  :: SyntaxState s => Syntax s String
h  = concatMany (comma) &+& (tolist1 . token (`elem` "'h")) &+& syn (nucleus)

digit  :: SyntaxState s => Syntax s String
digit  = concatMany (comma) &+& (tolist1 . token (`elem` "0123456789")) &+& notsyn (h) &+& notsyn (nucleus)

post_word  :: SyntaxState s => Syntax s String
post_word  = pause <+> notsyn (nucleus) &+& lojban_word

pause  :: SyntaxState s => Syntax s String
pause  = concatMany (comma) &+& concatSome (space_char)

comma  :: SyntaxState s => Syntax s String
comma  = (tolist1 . token (`elem` ","))

non_lojban_word  :: SyntaxState s => Syntax s String
non_lojban_word  = notsyn (lojban_word) &+& concatSome (non_space)

non_space  :: SyntaxState s => Syntax s String
non_space  = notsyn (space_char)

space_char  :: SyntaxState s => Syntax s String
space_char  = (tolist1 . token (`elem` ".\t\n\r?! "))

spaces  :: SyntaxState s => Syntax s String
spaces  = notsyn (class_Y) &+& concatSome (space_char)

ybu  :: SyntaxState s => Syntax s String
ybu  = class_Y &+& concatMany (space_char) &+& class_BU

lujvo  :: SyntaxState s => Syntax s String
lujvo  = notsyn (gismu) &+& notsyn (fuhivla) &+& brivla

class_A  :: SyntaxState s => Syntax s String
class_A  = syn (cmavo) &+& (a <+> e <+> j &+& i <+> o <+> u) &+& syn (post_word)

class_BAI  :: SyntaxState s => Syntax s String
class_BAI  = syn (cmavo) &+& (d &+& u &+& h &+& o <+> s &+& i &+& h &+& u <+> z &+& a &+& u <+> k &+& i &+& h &+& i <+> d &+& u &+& h &+& i <+> c &+& u &+& h &+& u <+> t &+& u &+& h &+& i <+> t &+& i &+& h &+& u <+> d &+& i &+& h &+& o <+> j &+& i &+& h &+& u <+> r &+& i &+& h &+& a <+> n &+& i &+& h &+& i <+> m &+& u &+& h &+& i <+> k &+& i &+& h &+& u <+> v &+& a &+& h &+& u <+> k &+& o &+& i <+> c &+& a &+& h &+& i <+> t &+& a &+& h &+& i <+> p &+& u &+& h &+& e <+> j &+& a &+& h &+& i <+> k &+& a &+& i <+> b &+& a &+& i <+> f &+& i &+& h &+& e <+> d &+& e &+& h &+& i <+> c &+& i &+& h &+& o <+> m &+& a &+& u <+> m &+& u &+& h &+& u <+> r &+& i &+& h &+& i <+> r &+& a &+& h &+& i <+> k &+& a &+& h &+& a <+> p &+& a &+& h &+& u <+> p &+& a &+& h &+& a <+> l &+& e &+& h &+& a <+> k &+& u &+& h &+& u <+> t &+& a &+& i <+> b &+& a &+& u <+> m &+& a &+& h &+& i <+> c &+& i &+& h &+& e <+> f &+& a &+& u <+> p &+& o &+& h &+& i <+> c &+& a &+& u <+> m &+& a &+& h &+& e <+> c &+& i &+& h &+& u <+> r &+& a &+& h &+& a <+> p &+& u &+& h &+& a <+> l &+& i &+& h &+& e <+> l &+& a &+& h &+& u <+> b &+& a &+& h &+& i <+> k &+& a &+& h &+& i <+> s &+& a &+& u <+> f &+& a &+& h &+& e <+> b &+& e &+& h &+& i <+> t &+& i &+& h &+& i <+> j &+& a &+& h &+& e <+> g &+& a &+& h &+& a <+> v &+& a &+& h &+& o <+> j &+& i &+& h &+& o <+> m &+& e &+& h &+& a <+> d &+& o &+& h &+& e <+> j &+& i &+& h &+& e <+> p &+& i &+& h &+& o <+> g &+& a &+& u <+> z &+& u &+& h &+& e <+> m &+& e &+& h &+& e <+> r &+& a &+& i) &+& syn (post_word)

class_BAhE  :: SyntaxState s => Syntax s String
class_BAhE  = syn (cmavo) &+& (b &+& a &+& h &+& e <+> z &+& a &+& h &+& e) &+& syn (post_word)

class_BE  :: SyntaxState s => Syntax s String
class_BE  = syn (cmavo) &+& (b &+& e) &+& syn (post_word)

class_BEI  :: SyntaxState s => Syntax s String
class_BEI  = syn (cmavo) &+& (b &+& e &+& i) &+& syn (post_word)

class_BEhO  :: SyntaxState s => Syntax s String
class_BEhO  = syn (cmavo) &+& (b &+& e &+& h &+& o) &+& syn (post_word)

class_BIhE  :: SyntaxState s => Syntax s String
class_BIhE  = syn (cmavo) &+& (b &+& i &+& h &+& e) &+& syn (post_word)

class_BIhI  :: SyntaxState s => Syntax s String
class_BIhI  = syn (cmavo) &+& (m &+& i &+& h &+& i <+> b &+& i &+& h &+& o <+> b &+& i &+& h &+& i) &+& syn (post_word)

class_BO  :: SyntaxState s => Syntax s String
class_BO  = syn (cmavo) &+& (b &+& o) &+& syn (post_word)

class_BOI  :: SyntaxState s => Syntax s String
class_BOI  = syn (cmavo) &+& (b &+& o &+& i) &+& syn (post_word)

class_BU  :: SyntaxState s => Syntax s String
class_BU  = syn (cmavo) &+& (b &+& u) &+& syn (post_word)

class_BY  :: SyntaxState s => Syntax s String
class_BY  = syn (cmavo) &+& (ybu <+> j &+& o &+& h &+& o <+> r &+& u &+& h &+& o <+> g &+& e &+& h &+& o <+> j &+& e &+& h &+& o <+> l &+& o &+& h &+& a <+> n &+& a &+& h &+& a <+> s &+& e &+& h &+& e <+> t &+& o &+& h &+& a <+> g &+& a &+& h &+& e <+> y &+& h &+& y <+> b &+& y <+> c &+& y <+> d &+& y <+> f &+& y <+> g &+& y <+> j &+& y <+> k &+& y <+> l &+& y <+> m &+& y <+> n &+& y <+> p &+& y <+> r &+& y <+> s &+& y <+> t &+& y <+> v &+& y <+> x &+& y <+> z &+& y) &+& syn (post_word)

class_CAhA  :: SyntaxState s => Syntax s String
class_CAhA  = syn (cmavo) &+& (c &+& a &+& h &+& a <+> p &+& u &+& h &+& i <+> n &+& u &+& h &+& o <+> k &+& a &+& h &+& e) &+& syn (post_word)

class_CAI  :: SyntaxState s => Syntax s String
class_CAI  = syn (cmavo) &+& (p &+& e &+& i <+> c &+& a &+& i <+> c &+& u &+& h &+& i <+> s &+& a &+& i <+> r &+& u &+& h &+& e) &+& syn (post_word)

class_CEI  :: SyntaxState s => Syntax s String
class_CEI  = syn (cmavo) &+& (c &+& e &+& i) &+& syn (post_word)

class_CEhE  :: SyntaxState s => Syntax s String
class_CEhE  = syn (cmavo) &+& (c &+& e &+& h &+& e) &+& syn (post_word)

class_CO  :: SyntaxState s => Syntax s String
class_CO  = syn (cmavo) &+& (c &+& o) &+& syn (post_word)

class_COI  :: SyntaxState s => Syntax s String
class_COI  = syn (cmavo) &+& (j &+& u &+& h &+& i <+> c &+& o &+& i <+> f &+& i &+& h &+& i <+> t &+& a &+& h &+& a <+> m &+& u &+& h &+& o <+> f &+& e &+& h &+& o <+> c &+& o &+& h &+& o <+> p &+& e &+& h &+& u <+> k &+& e &+& h &+& o <+> n &+& u &+& h &+& e <+> r &+& e &+& h &+& i <+> b &+& e &+& h &+& e <+> j &+& e &+& h &+& e <+> m &+& i &+& h &+& e <+> k &+& i &+& h &+& e <+> v &+& i &+& h &+& o) &+& syn (post_word)

class_CU  :: SyntaxState s => Syntax s String
class_CU  = syn (cmavo) &+& (c &+& u) &+& syn (post_word)

class_CUhE  :: SyntaxState s => Syntax s String
class_CUhE  = syn (cmavo) &+& (c &+& u &+& h &+& e <+> n &+& a &+& u) &+& syn (post_word)

class_DAhO  :: SyntaxState s => Syntax s String
class_DAhO  = syn (cmavo) &+& (d &+& a &+& h &+& o) &+& syn (post_word)

class_DOI  :: SyntaxState s => Syntax s String
class_DOI  = syn (cmavo) &+& (d &+& o &+& i) &+& syn (post_word)

class_DOhU  :: SyntaxState s => Syntax s String
class_DOhU  = syn (cmavo) &+& (d &+& o &+& h &+& u) &+& syn (post_word)

class_FA  :: SyntaxState s => Syntax s String
class_FA  = syn (cmavo) &+& (f &+& a &+& i <+> f &+& a <+> f &+& e <+> f &+& o <+> f &+& u <+> f &+& i &+& h &+& a <+> f &+& i) &+& syn (post_word)

class_FAhA  :: SyntaxState s => Syntax s String
class_FAhA  = syn (cmavo) &+& (d &+& u &+& h &+& a <+> b &+& e &+& h &+& a <+> n &+& e &+& h &+& u <+> v &+& u &+& h &+& a <+> g &+& a &+& h &+& u <+> t &+& i &+& h &+& a <+> n &+& i &+& h &+& a <+> c &+& a &+& h &+& u <+> z &+& u &+& h &+& a <+> r &+& i &+& h &+& u <+> r &+& u &+& h &+& u <+> r &+& e &+& h &+& o <+> t &+& e &+& h &+& e <+> b &+& u &+& h &+& u <+> n &+& e &+& h &+& a <+> p &+& a &+& h &+& o <+> n &+& e &+& h &+& i <+> t &+& o &+& h &+& o <+> z &+& o &+& h &+& i <+> z &+& e &+& h &+& o <+> z &+& o &+& h &+& a <+> f &+& a &+& h &+& a) &+& syn (post_word) &+& syn (post_word)

class_FAhO  :: SyntaxState s => Syntax s String
class_FAhO  = syn (cmavo) &+& (f &+& a &+& h &+& o) &+& syn (post_word)

class_FEhE  :: SyntaxState s => Syntax s String
class_FEhE  = syn (cmavo) &+& (f &+& e &+& h &+& e) &+& syn (post_word)

class_FEhU  :: SyntaxState s => Syntax s String
class_FEhU  = syn (cmavo) &+& (f &+& e &+& h &+& u) &+& syn (post_word)

class_FIhO  :: SyntaxState s => Syntax s String
class_FIhO  = syn (cmavo) &+& (f &+& i &+& h &+& o) &+& syn (post_word)

class_FOI  :: SyntaxState s => Syntax s String
class_FOI  = syn (cmavo) &+& (f &+& o &+& i) &+& syn (post_word)

class_FUhA  :: SyntaxState s => Syntax s String
class_FUhA  = syn (cmavo) &+& (f &+& u &+& h &+& a) &+& syn (post_word)

class_FUhE  :: SyntaxState s => Syntax s String
class_FUhE  = syn (cmavo) &+& (f &+& u &+& h &+& e) &+& syn (post_word)

class_FUhO  :: SyntaxState s => Syntax s String
class_FUhO  = syn (cmavo) &+& (f &+& u &+& h &+& o) &+& syn (post_word)

class_GA  :: SyntaxState s => Syntax s String
class_GA  = syn (cmavo) &+& (g &+& e &+& h &+& i <+> g &+& e <+> g &+& o <+> g &+& a <+> g &+& u) &+& syn (post_word)

class_GAhO  :: SyntaxState s => Syntax s String
class_GAhO  = syn (cmavo) &+& (k &+& e &+& h &+& i <+> g &+& a &+& h &+& o) &+& syn (post_word)

class_GEhU  :: SyntaxState s => Syntax s String
class_GEhU  = syn (cmavo) &+& (g &+& e &+& h &+& u) &+& syn (post_word)

class_GI  :: SyntaxState s => Syntax s String
class_GI  = syn (cmavo) &+& (g &+& i) &+& syn (post_word)

class_GIhA  :: SyntaxState s => Syntax s String
class_GIhA  = syn (cmavo) &+& (g &+& i &+& h &+& e <+> g &+& i &+& h &+& i <+> g &+& i &+& h &+& o <+> g &+& i &+& h &+& a <+> g &+& i &+& h &+& u) &+& syn (post_word)

class_GOI  :: SyntaxState s => Syntax s String
class_GOI  = syn (cmavo) &+& (n &+& o &+& h &+& u <+> n &+& e <+> g &+& o &+& i <+> p &+& o &+& h &+& u <+> p &+& e <+> p &+& o &+& h &+& e <+> p &+& o) &+& syn (post_word)

class_GOhA  :: SyntaxState s => Syntax s String
class_GOhA  = syn (cmavo) &+& (m &+& o <+> n &+& e &+& i <+> g &+& o &+& h &+& u <+> g &+& o &+& h &+& o <+> g &+& o &+& h &+& i <+> n &+& o &+& h &+& a <+> g &+& o &+& h &+& e <+> g &+& o &+& h &+& a <+> d &+& u <+> b &+& u &+& h &+& a <+> b &+& u &+& h &+& e <+> b &+& u &+& h &+& i <+> c &+& o &+& h &+& e) &+& syn (post_word)

class_GUhA  :: SyntaxState s => Syntax s String
class_GUhA  = syn (cmavo) &+& (g &+& u &+& h &+& e <+> g &+& u &+& h &+& i <+> g &+& u &+& h &+& o <+> g &+& u &+& h &+& a <+> g &+& u &+& h &+& u) &+& syn (post_word)

class_I  :: SyntaxState s => Syntax s String
class_I  = syn (cmavo) &+& (i) &+& syn (post_word)

class_JA  :: SyntaxState s => Syntax s String
class_JA  = syn (cmavo) &+& (j &+& e &+& h &+& i <+> j &+& e <+> j &+& o <+> j &+& a <+> j &+& u) &+& syn (post_word)

class_JAI  :: SyntaxState s => Syntax s String
class_JAI  = syn (cmavo) &+& (j &+& a &+& i) &+& syn (post_word)

class_JOhI  :: SyntaxState s => Syntax s String
class_JOhI  = syn (cmavo) &+& (j &+& o &+& h &+& i) &+& syn (post_word)

class_JOI  :: SyntaxState s => Syntax s String
class_JOI  = syn (cmavo) &+& (f &+& a &+& h &+& u <+> p &+& i &+& h &+& u <+> j &+& o &+& i <+> c &+& e &+& h &+& o <+> c &+& e <+> j &+& o &+& h &+& u <+> k &+& u &+& h &+& a <+> j &+& o &+& h &+& e <+> j &+& u &+& h &+& e) &+& syn (post_word)

class_KE  :: SyntaxState s => Syntax s String
class_KE  = syn (cmavo) &+& (k &+& e) &+& syn (post_word)

class_KEhE  :: SyntaxState s => Syntax s String
class_KEhE  = syn (cmavo) &+& (k &+& e &+& h &+& e) &+& syn (post_word)

class_KEI  :: SyntaxState s => Syntax s String
class_KEI  = syn (cmavo) &+& (k &+& e &+& i) &+& syn (post_word)

class_KI  :: SyntaxState s => Syntax s String
class_KI  = syn (cmavo) &+& (k &+& i) &+& syn (post_word)

class_KOhA  :: SyntaxState s => Syntax s String
class_KOhA  = syn (cmavo) &+& (d &+& a &+& h &+& u <+> d &+& a &+& h &+& e <+> d &+& i &+& h &+& u <+> d &+& i &+& h &+& e <+> d &+& e &+& h &+& u <+> d &+& e &+& h &+& e <+> d &+& e &+& i <+> d &+& o &+& h &+& i <+> m &+& i &+& h &+& o <+> m &+& a &+& h &+& a <+> m &+& i &+& h &+& a <+> d &+& o &+& h &+& o <+> k &+& o &+& h &+& a <+> f &+& o &+& h &+& u <+> k &+& o &+& h &+& e <+> k &+& o &+& h &+& i <+> k &+& o &+& h &+& o <+> k &+& o &+& h &+& u <+> f &+& o &+& h &+& a <+> f &+& o &+& h &+& e <+> f &+& o &+& h &+& i <+> f &+& o &+& h &+& o <+> v &+& o &+& h &+& a <+> v &+& o &+& h &+& e <+> v &+& o &+& h &+& i <+> v &+& o &+& h &+& o <+> v &+& o &+& h &+& u <+> r &+& u <+> r &+& i <+> r &+& a <+> t &+& a <+> t &+& u <+> t &+& i <+> z &+& i &+& h &+& o <+> k &+& e &+& h &+& a <+> m &+& a <+> z &+& u &+& h &+& i <+> z &+& o &+& h &+& e <+> c &+& e &+& h &+& u <+> d &+& a <+> d &+& e <+> d &+& i <+> k &+& o <+> m &+& i <+> d &+& o) &+& syn (post_word)

class_KU  :: SyntaxState s => Syntax s String
class_KU  = syn (cmavo) &+& (k &+& u) &+& syn (post_word)

class_KUhE  :: SyntaxState s => Syntax s String
class_KUhE  = syn (cmavo) &+& (k &+& u &+& h &+& e) &+& syn (post_word)

class_KUhO  :: SyntaxState s => Syntax s String
class_KUhO  = syn (cmavo) &+& (k &+& u &+& h &+& o) &+& syn (post_word)

class_LA  :: SyntaxState s => Syntax s String
class_LA  = syn (cmavo) &+& (l &+& a &+& i <+> l &+& a &+& h &+& i <+> l &+& a) &+& syn (post_word)

class_LAU  :: SyntaxState s => Syntax s String
class_LAU  = syn (cmavo) &+& (c &+& e &+& h &+& a <+> l &+& a &+& u <+> z &+& a &+& i <+> t &+& a &+& u) &+& syn (post_word)

class_LAhE  :: SyntaxState s => Syntax s String
class_LAhE  = syn (cmavo) &+& (t &+& u &+& h &+& a <+> l &+& u &+& h &+& a <+> l &+& u &+& h &+& o <+> l &+& a &+& h &+& e <+> v &+& u &+& h &+& i <+> l &+& u &+& h &+& i <+> l &+& u &+& h &+& e) &+& syn (post_word)

class_LE  :: SyntaxState s => Syntax s String
class_LE  = syn (cmavo) &+& (l &+& e &+& i <+> l &+& o &+& i <+> l &+& e &+& h &+& i <+> l &+& o &+& h &+& i <+> l &+& e &+& h &+& e <+> l &+& o &+& h &+& e <+> l &+& o <+> l &+& e) &+& syn (post_word)

class_LEhU  :: SyntaxState s => Syntax s String
class_LEhU  = syn (cmavo) &+& (l &+& e &+& h &+& u) &+& syn (post_word)

class_LI  :: SyntaxState s => Syntax s String
class_LI  = syn (cmavo) &+& (m &+& e &+& h &+& o <+> l &+& i) &+& syn (post_word)

class_LIhU  :: SyntaxState s => Syntax s String
class_LIhU  = syn (cmavo) &+& (l &+& i &+& h &+& u) &+& syn (post_word)

class_LOhO  :: SyntaxState s => Syntax s String
class_LOhO  = syn (cmavo) &+& (l &+& o &+& h &+& o) &+& syn (post_word)

class_LOhU  :: SyntaxState s => Syntax s String
class_LOhU  = syn (cmavo) &+& (l &+& o &+& h &+& u) &+& syn (post_word)

class_LU  :: SyntaxState s => Syntax s String
class_LU  = syn (cmavo) &+& (l &+& u) &+& syn (post_word)

class_LUhU  :: SyntaxState s => Syntax s String
class_LUhU  = syn (cmavo) &+& (l &+& u &+& h &+& u) &+& syn (post_word)

class_MAhO  :: SyntaxState s => Syntax s String
class_MAhO  = syn (cmavo) &+& (m &+& a &+& h &+& o) &+& syn (post_word)

class_MAI  :: SyntaxState s => Syntax s String
class_MAI  = syn (cmavo) &+& (m &+& o &+& h &+& o <+> m &+& a &+& i) &+& syn (post_word)

class_ME  :: SyntaxState s => Syntax s String
class_ME  = syn (cmavo) &+& (m &+& e) &+& syn (post_word)

class_MEhU  :: SyntaxState s => Syntax s String
class_MEhU  = syn (cmavo) &+& (m &+& e &+& h &+& u) &+& syn (post_word)

class_MOhE  :: SyntaxState s => Syntax s String
class_MOhE  = syn (cmavo) &+& (m &+& o &+& h &+& e) &+& syn (post_word)

class_MOhI  :: SyntaxState s => Syntax s String
class_MOhI  = syn (cmavo) &+& (m &+& o &+& h &+& i) &+& syn (post_word)

class_MOI  :: SyntaxState s => Syntax s String
class_MOI  = syn (cmavo) &+& (m &+& e &+& i <+> m &+& o &+& i <+> s &+& i &+& h &+& e <+> c &+& u &+& h &+& o <+> v &+& a &+& h &+& e) &+& syn (post_word)

class_NA  :: SyntaxState s => Syntax s String
class_NA  = syn (cmavo) &+& (j &+& a &+& h &+& a <+> n &+& a) &+& syn (post_word)

class_NAI  :: SyntaxState s => Syntax s String
class_NAI  = syn (cmavo) &+& (n &+& a &+& i) &+& syn (post_word)

class_NAhE  :: SyntaxState s => Syntax s String
class_NAhE  = syn (cmavo) &+& (t &+& o &+& h &+& e <+> j &+& e &+& h &+& a <+> n &+& a &+& h &+& e <+> n &+& o &+& h &+& e) &+& syn (post_word)

class_NAhU  :: SyntaxState s => Syntax s String
class_NAhU  = syn (cmavo) &+& (n &+& a &+& h &+& u) &+& syn (post_word)

class_NIhE  :: SyntaxState s => Syntax s String
class_NIhE  = syn (cmavo) &+& (n &+& i &+& h &+& e) &+& syn (post_word)

class_NIhO  :: SyntaxState s => Syntax s String
class_NIhO  = syn (cmavo) &+& (n &+& i &+& h &+& o <+> n &+& o &+& h &+& i) &+& syn (post_word)

class_NOI  :: SyntaxState s => Syntax s String
class_NOI  = syn (cmavo) &+& (v &+& o &+& i <+> n &+& o &+& i <+> p &+& o &+& i) &+& syn (post_word)

class_NU  :: SyntaxState s => Syntax s String
class_NU  = syn (cmavo) &+& (n &+& i <+> d &+& u &+& h &+& u <+> s &+& i &+& h &+& o <+> n &+& u <+> l &+& i &+& h &+& i <+> k &+& a <+> j &+& e &+& i <+> s &+& u &+& h &+& u <+> z &+& u &+& h &+& o <+> m &+& u &+& h &+& e <+> p &+& u &+& h &+& u <+> z &+& a &+& h &+& i) &+& syn (post_word)

class_NUhA  :: SyntaxState s => Syntax s String
class_NUhA  = syn (cmavo) &+& (n &+& u &+& h &+& a) &+& syn (post_word)

class_NUhI  :: SyntaxState s => Syntax s String
class_NUhI  = syn (cmavo) &+& (n &+& u &+& h &+& i) &+& syn (post_word)

class_NUhU  :: SyntaxState s => Syntax s String
class_NUhU  = syn (cmavo) &+& (n &+& u &+& h &+& u) &+& syn (post_word)

class_PA  :: SyntaxState s => Syntax s String
class_PA  = syn (cmavo) &+& (d &+& a &+& u <+> f &+& e &+& i <+> g &+& a &+& i <+> j &+& a &+& u <+> r &+& e &+& i <+> v &+& a &+& i <+> p &+& i &+& h &+& e <+> p &+& i <+> f &+& i &+& h &+& u <+> z &+& a &+& h &+& u <+> m &+& e &+& h &+& i <+> n &+& i &+& h &+& u <+> k &+& i &+& h &+& o <+> c &+& e &+& h &+& i <+> m &+& a &+& h &+& u <+> r &+& a &+& h &+& e <+> d &+& a &+& h &+& a <+> s &+& o &+& h &+& a <+> j &+& i &+& h &+& i <+> s &+& u &+& h &+& o <+> s &+& u &+& h &+& e <+> r &+& o <+> r &+& a &+& u <+> s &+& o &+& h &+& u <+> s &+& o &+& h &+& i <+> s &+& o &+& h &+& e <+> s &+& o &+& h &+& o <+> m &+& o &+& h &+& a <+> d &+& u &+& h &+& e <+> t &+& e &+& h &+& o <+> k &+& a &+& h &+& o <+> c &+& i &+& h &+& i <+> t &+& u &+& h &+& o <+> x &+& o <+> p &+& a &+& i <+> n &+& o &+& h &+& o <+> n &+& o <+> p &+& a <+> r &+& e <+> c &+& i <+> v &+& o <+> m &+& u <+> x &+& a <+> z &+& e <+> b &+& i <+> s &+& o <+> digit) &+& syn (post_word)

class_PEhE  :: SyntaxState s => Syntax s String
class_PEhE  = syn (cmavo) &+& (p &+& e &+& h &+& e) &+& syn (post_word)

class_PEhO  :: SyntaxState s => Syntax s String
class_PEhO  = syn (cmavo) &+& (p &+& e &+& h &+& o) &+& syn (post_word)

class_PU  :: SyntaxState s => Syntax s String
class_PU  = syn (cmavo) &+& (b &+& a <+> p &+& u <+> c &+& a) &+& syn (post_word)

class_RAhO  :: SyntaxState s => Syntax s String
class_RAhO  = syn (cmavo) &+& (r &+& a &+& h &+& o) &+& syn (post_word)

class_ROI  :: SyntaxState s => Syntax s String
class_ROI  = syn (cmavo) &+& (r &+& e &+& h &+& u <+> r &+& o &+& i) &+& syn (post_word)

class_SA  :: SyntaxState s => Syntax s String
class_SA  = syn (cmavo) &+& (s &+& a) &+& syn (post_word)

class_SE  :: SyntaxState s => Syntax s String
class_SE  = syn (cmavo) &+& (s &+& e <+> t &+& e <+> v &+& e <+> x &+& e) &+& syn (post_word)

class_SEI  :: SyntaxState s => Syntax s String
class_SEI  = syn (cmavo) &+& (s &+& e &+& i <+> t &+& i &+& h &+& o) &+& syn (post_word)

class_SEhU  :: SyntaxState s => Syntax s String
class_SEhU  = syn (cmavo) &+& (s &+& e &+& h &+& u) &+& syn (post_word)

class_SI  :: SyntaxState s => Syntax s String
class_SI  = syn (cmavo) &+& (s &+& i) &+& syn (post_word)

class_SOI  :: SyntaxState s => Syntax s String
class_SOI  = syn (cmavo) &+& (s &+& o &+& i) &+& syn (post_word)

class_SU  :: SyntaxState s => Syntax s String
class_SU  = syn (cmavo) &+& (s &+& u) &+& syn (post_word)

class_TAhE  :: SyntaxState s => Syntax s String
class_TAhE  = syn (cmavo) &+& (r &+& u &+& h &+& i <+> t &+& a &+& h &+& e <+> d &+& i &+& h &+& i <+> n &+& a &+& h &+& o) &+& syn (post_word)

class_TEhU  :: SyntaxState s => Syntax s String
class_TEhU  = syn (cmavo) &+& (t &+& e &+& h &+& u) &+& syn (post_word)

class_TEI  :: SyntaxState s => Syntax s String
class_TEI  = syn (cmavo) &+& (t &+& e &+& i) &+& syn (post_word)

class_TO  :: SyntaxState s => Syntax s String
class_TO  = syn (cmavo) &+& (t &+& o &+& h &+& i <+> t &+& o) &+& syn (post_word)

class_TOI  :: SyntaxState s => Syntax s String
class_TOI  = syn (cmavo) &+& (t &+& o &+& i) &+& syn (post_word)

class_TUhE  :: SyntaxState s => Syntax s String
class_TUhE  = syn (cmavo) &+& (t &+& u &+& h &+& e) &+& syn (post_word)

class_TUhU  :: SyntaxState s => Syntax s String
class_TUhU  = syn (cmavo) &+& (t &+& u &+& h &+& u) &+& syn (post_word)

class_UI  :: SyntaxState s => Syntax s String
class_UI  = syn (cmavo) &+& (i &+& h &+& a <+> i &+& e <+> a &+& h &+& e <+> u &+& h &+& i <+> i &+& h &+& o <+> i &+& h &+& e <+> a &+& h &+& a <+> i &+& a <+> o &+& h &+& i <+> o &+& h &+& e <+> e &+& h &+& e <+> o &+& i <+> u &+& o <+> e &+& h &+& i <+> u &+& h &+& o <+> a &+& u <+> u &+& a <+> a &+& h &+& i <+> i &+& h &+& u <+> i &+& i <+> u &+& h &+& a <+> u &+& i <+> a &+& h &+& o <+> a &+& i <+> a &+& h &+& u <+> i &+& u <+> e &+& i <+> o &+& h &+& o <+> e &+& h &+& a <+> u &+& u <+> o &+& h &+& a <+> o &+& h &+& u <+> u &+& h &+& u <+> e &+& h &+& o <+> i &+& o <+> e &+& h &+& u <+> u &+& e <+> i &+& h &+& i <+> u &+& h &+& e <+> b &+& a &+& h &+& a <+> j &+& a &+& h &+& o <+> c &+& a &+& h &+& e <+> s &+& u &+& h &+& a <+> t &+& i &+& h &+& e <+> k &+& a &+& h &+& u <+> s &+& e &+& h &+& o <+> z &+& a &+& h &+& a <+> p &+& e &+& h &+& i <+> r &+& u &+& h &+& a <+> j &+& u &+& h &+& a <+> t &+& a &+& h &+& o <+> r &+& a &+& h &+& u <+> l &+& i &+& h &+& a <+> b &+& a &+& h &+& u <+> m &+& u &+& h &+& a <+> d &+& o &+& h &+& a <+> t &+& o &+& h &+& u <+> v &+& a &+& h &+& i <+> p &+& a &+& h &+& e <+> z &+& u &+& h &+& u <+> s &+& a &+& h &+& e <+> l &+& a &+& h &+& a <+> k &+& e &+& h &+& u <+> s &+& a &+& h &+& u <+> d &+& a &+& h &+& i <+> j &+& e &+& h &+& u <+> s &+& a &+& h &+& a <+> k &+& a &+& u <+> t &+& a &+& h &+& u <+> n &+& a &+& h &+& i <+> j &+& o &+& h &+& a <+> b &+& i &+& h &+& u <+> l &+& i &+& h &+& o <+> p &+& a &+& u <+> m &+& i &+& h &+& u <+> k &+& u &+& h &+& i <+> j &+& i &+& h &+& a <+> s &+& i &+& h &+& a <+> p &+& o &+& h &+& o <+> p &+& e &+& h &+& a <+> r &+& o &+& h &+& i <+> r &+& o &+& h &+& e <+> r &+& o &+& h &+& o <+> r &+& o &+& h &+& u <+> r &+& o &+& h &+& a <+> r &+& e &+& h &+& e <+> l &+& e &+& h &+& o <+> j &+& u &+& h &+& o <+> f &+& u &+& h &+& i <+> d &+& a &+& i <+> g &+& a &+& h &+& i <+> z &+& o &+& h &+& o <+> b &+& e &+& h &+& u <+> r &+& i &+& h &+& e <+> s &+& e &+& h &+& i <+> s &+& e &+& h &+& a <+> v &+& u &+& h &+& e <+> k &+& i &+& h &+& a <+> x &+& u <+> g &+& e &+& h &+& e <+> b &+& u &+& h &+& o) &+& syn (post_word)

class_VA  :: SyntaxState s => Syntax s String
class_VA  = syn (cmavo) &+& (v &+& i <+> v &+& a <+> v &+& u) &+& syn (post_word)

class_VAU  :: SyntaxState s => Syntax s String
class_VAU  = syn (cmavo) &+& (v &+& a &+& u) &+& syn (post_word)

class_VEI  :: SyntaxState s => Syntax s String
class_VEI  = syn (cmavo) &+& (v &+& e &+& i) &+& syn (post_word)

class_VEhO  :: SyntaxState s => Syntax s String
class_VEhO  = syn (cmavo) &+& (v &+& e &+& h &+& o) &+& syn (post_word)

class_VUhU  :: SyntaxState s => Syntax s String
class_VUhU  = syn (cmavo) &+& (g &+& e &+& h &+& a <+> f &+& u &+& h &+& u <+> p &+& i &+& h &+& i <+> f &+& e &+& h &+& i <+> v &+& u &+& h &+& u <+> s &+& u &+& h &+& i <+> j &+& u &+& h &+& u <+> g &+& e &+& i <+> p &+& a &+& h &+& i <+> f &+& a &+& h &+& i <+> t &+& e &+& h &+& a <+> c &+& u &+& h &+& a <+> v &+& a &+& h &+& a <+> n &+& e &+& h &+& o <+> d &+& e &+& h &+& o <+> f &+& e &+& h &+& a <+> s &+& a &+& h &+& o <+> r &+& e &+& h &+& a <+> r &+& i &+& h &+& o <+> s &+& a &+& h &+& i <+> p &+& i &+& h &+& a <+> s &+& i &+& h &+& i) &+& syn (post_word)

class_VEhA  :: SyntaxState s => Syntax s String
class_VEhA  = syn (cmavo) &+& (v &+& e &+& h &+& u <+> v &+& e &+& h &+& a <+> v &+& e &+& h &+& i <+> v &+& e &+& h &+& e) &+& syn (post_word)

class_VIhA  :: SyntaxState s => Syntax s String
class_VIhA  = syn (cmavo) &+& (v &+& i &+& h &+& i <+> v &+& i &+& h &+& a <+> v &+& i &+& h &+& u <+> v &+& i &+& h &+& e) &+& syn (post_word)

class_VUhO  :: SyntaxState s => Syntax s String
class_VUhO  = syn (cmavo) &+& (v &+& u &+& h &+& o) &+& syn (post_word)

class_XI  :: SyntaxState s => Syntax s String
class_XI  = syn (cmavo) &+& (x &+& i) &+& syn (post_word)

class_Y  :: SyntaxState s => Syntax s String
class_Y  = syn (cmavo) &+& (concatSome (y)) &+& syn (post_word)

class_ZAhO  :: SyntaxState s => Syntax s String
class_ZAhO  = syn (cmavo) &+& (c &+& o &+& h &+& i <+> p &+& u &+& h &+& o <+> c &+& o &+& h &+& u <+> m &+& o &+& h &+& u <+> c &+& a &+& h &+& o <+> c &+& o &+& h &+& a <+> d &+& e &+& h &+& a <+> b &+& a &+& h &+& o <+> d &+& i &+& h &+& a <+> z &+& a &+& h &+& o) &+& syn (post_word)

class_ZEhA  :: SyntaxState s => Syntax s String
class_ZEhA  = syn (cmavo) &+& (z &+& e &+& h &+& u <+> z &+& e &+& h &+& a <+> z &+& e &+& h &+& i <+> z &+& e &+& h &+& e) &+& syn (post_word)

class_ZEI  :: SyntaxState s => Syntax s String
class_ZEI  = syn (cmavo) &+& (z &+& e &+& i) &+& syn (post_word)

class_ZI  :: SyntaxState s => Syntax s String
class_ZI  = syn (cmavo) &+& (z &+& u <+> z &+& a <+> z &+& i) &+& syn (post_word)

class_ZIhE  :: SyntaxState s => Syntax s String
class_ZIhE  = syn (cmavo) &+& (z &+& i &+& h &+& e) &+& syn (post_word)

class_ZO  :: SyntaxState s => Syntax s String
class_ZO  = syn (cmavo) &+& (z &+& o) &+& syn (post_word)

class_ZOI  :: SyntaxState s => Syntax s String
class_ZOI  = syn (cmavo) &+& (z &+& o &+& i <+> l &+& a &+& h &+& o) &+& syn (post_word)

class_ZOhU  :: SyntaxState s => Syntax s String
class_ZOhU  = syn (cmavo) &+& (z &+& o &+& h &+& u) &+& syn (post_word)

