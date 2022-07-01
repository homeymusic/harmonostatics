common_scales <- function() {
  c(diatonic_scales(),pentatonic_scales(),chromatic_scale())
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
ionian_tonic_chords <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii"=c(4,7,11),
       "IV"=c(5,9,12),
       "V"=c(7,11,14),
       "vi"=c(9,12,16),
       "vii*"=c(11,14,17)
  )
}
ionian_tonic_chords.0 <- function() {
  list("I"=c(0,4,7),
       "ii"=c(2,5,9),
       "iii"=c(4,7,11),
       "IV"=c(5,9,12),
       "V"=c(7,11,2),
       "vi"=c(9,12,4),
       "vii*"=c(11,2,5)
  )
}
phrygian_tonic_chords <- function() {
  list("i"=c(0,3,7),
       "II"=c(1,5,8),
       "III"=c(3,7,10),
       "iv"=c(5,8,12),
       "v*"=c(7,10,13),
       "VI"=c(8,12,15),
       "vii"=c(10,13,17)
  )
}
intervals_list <- function() {
  tbl = intervals.0()
  setNames(as.list(tbl$semitone), tbl$name)
}
