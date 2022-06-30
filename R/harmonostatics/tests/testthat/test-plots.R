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

test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})

test_that("potential energy plots look good", {
  p = plot_harmony(x=0:12,home=0,columns=c("semitone","potential_energy"))
  expect(p, "plot is probably ok")
  p = plot_harmony(x=0:12,home=0,columns=c("affinity","potential_energy"))
  expect(p, "plot is probably ok")
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
  p = plot_harmony(common_scales()['locrian'],0,unlist=TRUE,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['dorian'],0,unlist=TRUE,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['lydian'],0,unlist=TRUE,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
})

