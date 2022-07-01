common_scales <- function() {
  list("lydian"=c(0,2,4,6,7,9,11,12),
       "ionian"=c(0,2,4,5,7,9,11,12),
       "mixolydian"=c(0,2,4,5,7,9,10,12),
       "dorian"=c(0,2,3,5,7,9,10,12),
       "aeolian"=c(0,2,3,5,7,8,10,12),
       "phrygian"=c(0,1,3,5,7,8,10,12),
       "locrian"=c(0,1,3,5,6,8,10,12),
       "chromatic"=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
       "major pentatonic"=c(0,2,4,7,9,12),
       "blues major pentatonic"=c(0,2,5,7,9,12),
       "suspended pentatonic"=c(0,2,5,7,10,12),
       "minor pentatonic"=c(0,3,5,7,10,12),
       "blues minor pentatonic"=c(0,3,5,8,10,12))
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
test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})

test_that("affinity brightness plots look good", {
  p = plot_harmony(-12:0,0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(0:12,0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(12:24,0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales(),0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['locrian'],0,unlist=TRUE,columns=c("brightness","affinity"),title='locrian')
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['dorian'],0,unlist=TRUE,columns=c("brightness","affinity"),title='dorian')
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['lydian'],0,unlist=TRUE,columns=c("brightness","affinity"),title='lydian')
  expect(p, "plot is probably ok")
})

test_that("potential energy look good", {
  p = plot_harmony(ionian_tonic_chords(),0,home_chord=c(0,4,7),columns=c("brightness","potential_energy"),title="ionian tonic chords w home")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords(),0,home_chord=c(0,4,7),columns=c("semitone","potential_energy"),title="ionian tonic chords w home")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords(),0,columns=c("semitone","potential_energy"),title="ionian tonic chords w/o home")
  expect(p, "plot is probably ok")
  p = plot_harmony(x=0:12,home=0,columns=c("semitone","potential_energy"),title="Tonic Potential Energy 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(x=0:12,home=12,columns=c("semitone","potential_energy"),title="Octave Potential Energy 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords(),0,columns=c("magnitude","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords.0(),0,columns=c("semitone","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords.0(),0,columns=c("magnitude","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
})
test_that("phrygian chords look good", {
  p = plot_harmony(phrygian_tonic_chords(),0,columns=c("semitone","potential_energy"),title="phrygian tonic chords")
  expect(p, "plot is probably ok")
})
