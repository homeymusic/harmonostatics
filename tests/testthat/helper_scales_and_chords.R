common_scales <- function() {
  c(diatonic_scales(),non_diatonic_heptatonic_scales(),pentatonic_scales(),chromatic_scale())
}
# order by increasing brightness
diatonic_scales <- function() {
  list(
    "locrian"=c(0,1,3,5,6,8,10,12),
    "phrygian"=c(0,1,3,5,7,8,10,12),
    "aeolian"=c(0,2,3,5,7,8,10,12),
    "dorian"=c(0,2,3,5,7,9,10,12),
    "mixolydian"=c(0,2,4,5,7,9,10,12),
    "ionian"=c(0,2,4,5,7,9,11,12),
    "lydian"=c(0,2,4,6,7,9,11,12)
  )
}
non_diatonic_heptatonic_scales <- function() {
  list(
    "melodic minor"=c(0,2,3,5,7,9,11,12),
    "harmonic minor"=c(0,2,3,5,7,8,11,12),
    "neapolitan minor"=c(0,1,3,5,7,8,11,12),
    "neapolitan major"=c(0,1,3,5,7,9,11,12),
    "hungarian minor"=c(0,2,3,6,7,8,11,12),
    "phrygian dominant"=c(0,1,4,5,7,8,10,12)
  )
}
pentatonic_scales <- function() {
  list("major pentatonic"=c(0,2,4,7,9,12),
       "blues major pentatonic"=c(0,2,5,7,9,12),
       "suspended pentatonic"=c(0,2,5,7,10,12),
       "minor pentatonic"=c(0,3,5,7,10,12),
       "blues minor pentatonic"=c(0,3,5,8,10,12))
}
chromatic_scale <- function() {
  list("chromatic"=c(0,1,2,3,4,5,6,7,8,9,10,11,12))
}
#################
# tonic chords
#
lydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "II"=c(2,6,9),
       "iii"=c(4,7,11),
       "iv\u00B0"=c(6,9,12),
       "V"=c(7,11,14),
       "vi"=c(9,12,16),
       "vii"=c(11,14,18)
  )
}
ionian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii"=c(4,7,11),
       "IV"=c(5,9,12),
       "V"=c(7,11,14),
       "vi"=c(9,12,16),
       "vii\u00B0"=c(11,14,17)
  )
}
mixolydian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii\u00B0"=c(4,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi"=c(9,12,16),
       "VII"=c(10,14,17)
  )
}
dorian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii"=c(2,5,9),
       "III"=c(3,7,10),
       "IV"=c(5,9,12),
       "v"=c(7,10,14),
       "vi\u00B0"=c(9,12,15),
       "VII"=c(10,14,17)
  )
}
aeolian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "ii\u00B0"=c(2,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v"=c(7,10,14),
       "VI"=c(8,12,15),
       "VII"=c(10,14,17)
  )
}
phrygian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "II"=c(1,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v\u00B0"=c(7,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
locrian_tonic_chords <- function() {
  list("i\u00B0"=c(0,3,6),
       "II"=c(1,5,8),
       "iii"=c(3,6,10),
       "iv"=c(5,8,12),
       "V"=c(6,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
#################
# octave chords
#
dorian_octave_chords <- function() {
  list("VIII"=c(12,9,5),
       "VII"=c(10,7,3),
       "vi"=c(9,5,2),
       "v"=c(7,3,0),
       "IV"=c(5,2,-2),
       "III\u00B0"=c(3,0,-3),
       "i"=c(2,-2,-5)
  )
}
aeolian_octave_chords <- function() {
  list("viii"=c(12,8,5),
       "VII"=c(10,7,3),
       "VI\u00B0"=c(8,5,2),
       "v"=c(7,3,0),
       "IV"=c(5,2,-2),
       "III"=c(3,0,-4),
       "ii"=c(2,-2,-5)
  )
}
phrygian_octave_chords <- function() {
  list("viii"=c(12,8,5),
       "VII"=c(3,7,10),
       "VI"=c(1,5,8),
       "v"=c(0,3,7),
       "iv"=c(5,1,-2),
       "III"=c(3,0,-4),
       "II\u00B0"=c(1,-2,-5)
  )
}
locrian_octave_chords <- function() {
  list("viii"=c(12,8,5),
       "vii"=c(10,6,3),
       "VI"=c(8,5,1),
       "v\u00B0"=c(6,3,0),
       "iv"=c(5,1,-2),
       "III"=c(3,0,-4),
       "II"=c(1,-2,-6)
  )
}
#################
# one level chords
#
ionian_tonic_chords.in_one_level <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii"=c(4,7,11),
       "IV"=c(5,9,12),
       "V"=c(7,11,2),
       "vi"=c(9,12,4),
       "vii\u00B0"=c(11,2,5)
  )
}
#################
# sixth chords
#
mixolydian_tonic_sixth_chords <- function() {
  list("I"=c(0,5,9),
       "ii"=c(2,7,10),
       "iii\u00B0"=c(4,9,12),
       "IV"=c(5,10,14),
       "V"=c(7,12,16),
       "vi\u00B0"=c(9,14,17),
       "VII"=c(10,15,19)
  )
}
intervals_list <- function() {
  tbl = intervals()
  setNames(as.list(tbl$semitone), tbl$name)
}
