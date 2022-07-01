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
  p = plot_harmony(ionian_tonic_chords(),0,home_chord=c(0,4,7),columns=c("brightness","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords(),0,home_chord=c(0,4,7),columns=c("semitone","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
  p = plot_harmony(x=0:12,home=0,columns=c("semitone","potential_energy"),title="Tonic Potential Energy 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(x=0:12,home=12,columns=c("semitone","potential_energy"),title="Octave Potential Energy 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords.0(),0,home_chord=c(0,4,7),columns=c("semitone","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(ionian_tonic_chords.0(),0,home_chord=c(0,4,7),columns=c("brightness","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
})
test_that("phrygian chords look good", {
  p = plot_harmony(phrygian_tonic_chords(),0,columns=c("semitone","potential_energy"),title="phrygian tonic chords")
  expect(p, "plot is probably ok")
})
