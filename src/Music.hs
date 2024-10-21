module Music (Music, MusicList, name, genre, artist, bpm, difficulty, directory, chart, sounds, current, prev, next, selectAnother, selectNormal, isAnother, list, normalizeName) where

import Data.IntMap (IntMap)
import Data.List.Extra (trim)
import Data.Tuple.Extra (second3)
import qualified Music.Sounds as Sounds

type MusicList = ([Select], (Int, Music, Select), [Select])

data Select = Select {normal :: Music, another :: Maybe Music}

data Music = Music
  { name :: String
  , genre :: String
  , artist :: String
  , bpm :: Float
  , difficulty :: Maybe Int
  , directory :: FilePath
  , chart :: FilePath
  , sounds :: IntMap FilePath
  }

instance Eq Music where
  m1 == m2 = m1.name == m2.name

normalizeName :: String -> String
normalizeName = unwords . map trim . lines

current :: MusicList -> (Int, Music)
current (_, (i, c, _), _) = (i, c)

prev :: MusicList -> MusicList
prev (a : b, (i, _, c), d) = (b, (i - 1, a.normal, a), c : d)
prev x = x

next :: MusicList -> MusicList
next (a, (i, _, b), c : d) = (b : a, (i + 1, c.normal, c), d)
next x = x

selectAnother :: MusicList -> MusicList
selectAnother ml@(_, (i, _, m), _) = case m.another of
  Just a -> second3 (const (i, a, m)) ml
  Nothing -> ml

selectNormal :: MusicList -> MusicList
selectNormal ml@(_, (i, _, m), _) = second3 (const (i, m.normal, m)) ml

isAnother :: MusicList -> Bool
isAnother (_, (_, m, s), _) = m /= s.normal

list :: MusicList
list =
  ( []
  , (0, frozen, Select frozen Nothing)
  ,
    [ Select zionia Nothing
    , Select grave (Just graveA)
    , Select abacyber Nothing
    , Select platinum Nothing
    , Select llr Nothing
    , Select ld Nothing
    , Select empire (Just empireA)
    , Select a3 (Just a3A)
    , Select a2 (Just a2A)
    , Select ark (Just arkA)
    , Select wanderer Nothing
    , Select witchcraft (Just witchcraftA)
    , Select l9 Nothing
    , Select _2002 (Just _2002A)
    , Select binary Nothing
    ]
  )

frozen, zionia, grave, graveA, abacyber, platinum, llr, ld, empire, empireA, a3, a3A, a2, a2A, ark, arkA, wanderer, witchcraft, witchcraftA, l9, _2002, _2002A, binary :: Music
frozen = Music "Frozen Bond" "ballade" "paraoka" 106 (Just 1) "frozen" "frozen.ttr" Sounds.frozen
zionia = Music "Zionia Garden" "progressive" "paraoka" 172 (Just 2) "zionia" "zionia.ttr" Sounds.frozen
grave = Music "rainy\n   graveyard" "fold groove" "paraoka" 115 (Just 2) "grave" "grave.ttr" Sounds.grave
graveA = Music "rainy graveyard\n(autumn-taste)" "folk groove" "paraoka\nRemix:doctorR" 115 Nothing "grave" "grave_a.ttr" Sounds.graveA
abacyber = Music "ABANDONED\n       CYBER" "digi-rock" "Remixe:paraoka" 155 (Just 3) "abacyber" "abacyber.ttr" Sounds.abacyber
platinum = Music "platinum\n      garden" "breakbeats" "paraoka" 152 (Just 3) "platinum" "platinum.ttr" Sounds.platinum
llr = Music "LLR" "E-J" "paraoka" 170 (Just 3) "llr" "llr.ttr" Sounds.llr
ld = Music "lastdance" "fusion" "paraoka" 112 (Just 4) "ld" "ld.ttr" Sounds.ld
empire = Music "empire of\n     the steel" "minimal" "paraoka" 153 (Just 4) "empire" "empire.ttr" Sounds.empire
empireA = Music "empire of\n     the steam" "techno" "paraoka" 153 Nothing "empire" "empire_a.ttr" Sounds.empireA
a3 = Music "Acid-Ace\n     Allicance" "speedfunk" "paraoka" 147 (Just 4) "a3" "a3.ttr" Sounds.a3
a3A = Music "A3\n(deep mix)" "house" "paraoka\nRemixed:doctor.R" 131 Nothing "a3" "a3_a.ttr" Sounds.a3A
a2 = Music "A2" "psychedelic rock" "paraoka" 156 (Just 5) "a2" "a2.ttr" Sounds.a2
a2A = Music "A2 (sprit\n     flow mix)" "psychedelic punk" "paraoka\nRemixed:by doctor.R" 156 Nothing "a2" "a2_a.ttr" Sounds.a2A
ark = Music "Ark" "E-J" "paraoka" 160 (Just 5) "ark" "ark.ttr" Sounds.ark
arkA = Music "Ark\n(acid mix)" "E-J" "paraoka\nRemix:doctorR" 168 Nothing "ark" "ark_a.ttr" Sounds.arkA
wanderer = Music "wanderer's\n       destiny" "rock" "paraoka" 122 (Just 6) "wanderer" "wanderer.ttr" Sounds.wanderer
witchcraft = Music "witchcraft" "bigbeat" "paraoka" 115 (Just 6) "witchcraft" "witchcraft.ttr" Sounds.witchcraft
witchcraftA = Music "witchcraft\n(Another)" "bigbeat(7key)" "paraoka" 115 Nothing "witchcraft" "witchcraft_a.ttr" Sounds.witchcraftA
l9 = Music "L9" "Progressive" "paraoka" 123 Nothing "l9" "l9.ttr" Sounds.l9
_2002 = Music "2002,november" "&" "dj nagureo /\n     Yamajet" 128 Nothing "2002nov" "2002nov.ttr" Sounds._2002
_2002A = Music "2002,november\n(softlanding mix)" "&" "dj nagureo /\n     Yamajet" 2002 Nothing "2002nov" "2002nov_a.ttr" Sounds._2002A
binary = Music "Binary Numbers" "Digital Techno" "Cyclone Area" 94 Nothing "binary" "binary.ttr" Sounds.binary