module Music (Music, name, genre, artist, bpm, directory, chart, sounds) where

import Data.IntMap (IntMap)
import qualified Music.Sounds as Sounds

data Music = Music
  { name :: String
  , genre :: String
  , artist :: String
  , bpm :: Float
  , directory :: FilePath
  , chart :: FilePath
  , sounds :: IntMap FilePath
  }

frozen, zionia, grave, graveA, abacyber, platinum, llr, ld, empire, empireA, a3, a3A, a2, a2A, ark, arkA, wanderer, witchcraft, witchcraftA, l9, _2002, _2002A, binary :: Music
frozen = Music "Frozen Bond" "ballade" "paraoka" 106 "frozen" "frozen.ttr" Sounds.frozen
zionia = Music "Zionia Garden" "progressive" "paraoka" 172 "zionia" "zionia.ttr" Sounds.frozen
grave = Music "rainy graveyard" "fold groove" "paraoka" 115 "grave" "grave.ttr" Sounds.grave
graveA = Music "rg (autumn-taste)" "folk groove" "paraoka/doctorR" 115 "grave" "grave_a.ttr" Sounds.graveA
abacyber = Music "" "digi-rock" "remixed Paraoka" 155 "abacyber" "abacyber.ttr" Sounds.abacyber
platinum = Music "platinum garden" "breakbeats" "paraoka" 152 "platinum" "platinum.ttr" Sounds.platinum
llr = Music "LLR" "E-J" "paraoka" 170 "llr" "llr.ttr" Sounds.llr
ld = Music "lastdance" "fusion" "paraoka" 112 "ld" "ld.ttr" Sounds.ld
empire = Music "empire of the steel" "minimal" "paraoka" 153 "empire" "empire.ttr" Sounds.empire
empireA = Music "empire of the steam" "techno" "paraoka" 153 "empire" "empire_a.ttr" Sounds.empireA
a3 = Music "A3" "speedfunk" "paraoka" 147 "a3" "a3.ttr" Sounds.a3
a3A = Music "AcidAce-Alliance(deep mix)" "house" "paraoka Remixed by doctor.R" 131 "a3" "a3_a.ttr" Sounds.a3A
a2 = Music "A2" "psychedelic rock" "paraoka" 156 "a2" "a2.ttr" Sounds.a2
a2A = Music "A2(sprit flow mix)" "psychedelic punk" "paraoka Remixed by doctor.R" 156 "a2" "a2_a.ttr" Sounds.a2A
ark = Music "Ark" "E-J" "paraoka" 160 "ark" "ark.ttr" Sounds.ark
arkA = Music "Ark(acid mix)" "E-J" "paraoka/doctorR" 168 "ark" "ark_a.ttr" Sounds.arkA
wanderer = Music "wanderer's destiny" "rock" "paraoka" 122 "wanderer" "wanderer.ttr" Sounds.wanderer
witchcraft = Music "witchcraft" "bigbeat" "paraoka" 115 "witchcraft" "witchcraft.ttr" Sounds.witchcraft
witchcraftA = Music "witchcraft(another)" "bigbeat(7key)" "paraoka" 115 "witchcraft" "witchcraft_a.ttr" Sounds.witchcraftA
l9 = Music "L9" "Progressive" "paraoka" 123 "l9" "l9.ttr" Sounds.l9
_2002 = Music "2002,november" "&" "dj nagureo / Yamajet" 128 "2002nov" "2002nov.ttr" Sounds._2002
_2002A = Music "2002,november (softlanding mix)" "&" "dj nagureo / Yamajet" 2002 "2002nov" "2002nov_a.ttr" Sounds._2002A
binary = Music "Binary Numbers" "Digital Techno" "Cyclone Area" 94 "binary" "binary.ttr" Sounds.binary