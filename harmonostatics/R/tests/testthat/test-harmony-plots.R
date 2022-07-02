test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  text(affinity.0.octave(),affinity.0.tonic(),labels=names(intervals_list()),pos=3)

  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})

test_that("affinity brightness plots look good", {
  p = plot_harmony(list("level:-1"=-12:0),unlist=TRUE,home=0,columns=c("brightness","affinity"),include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  title = "Level 1 Intervals"
  p = plot_harmony_homey(intervals_list(),home=0,columns=c("brightness","affinity"), title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
  p = plot_harmony(list("level:+1"=12:24),unlist=TRUE,home=0,columns=c("brightness","affinity"),include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales(),home=0,columns=c("brightness","affinity"))
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['locrian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title='locrian',include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['dorian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title='dorian',include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_harmony(common_scales()['lydian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title='lydian',include_names=FALSE)
  expect(p, "plot is probably ok")
})

test_that("all one note pitches look good", {
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title="all 1 note pitches in level 0")
  expect(p, "plot is probably ok")
})
test_that("all two note chords look good", {
  p = plot_harmony(combn(0:12,2,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 2 note chords in level 0 tonic home")
  expect(p, "plot is probably ok")
  p = plot_harmony(combn(0:12,2,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 2 note chords in level 0 octave home")
  expect(p, "plot is probably ok")
})
test_that("all three note chords look good", {
  p = plot_harmony(combn(0:12,3,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 3 note chords in level 0 home 0")
  expect(p, "plot is probably ok")
  p = plot_harmony(combn(0:12,3,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 3 note chords in level 0 home 12")
  expect(p, "plot is probably ok")
})
test_that("all four note chords look good", {
  p = plot_harmony(combn(0:12,4,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 4 note chords in level 0")
  expect(p, "plot is probably ok")
})
test_that("all five note chords look good", {
  p = plot_harmony(combn(0:12,5,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 5 note chords in level 0")
  expect(p, "plot is probably ok")
})
test_that("all six note chords look good", {
  p = plot_harmony(combn(0:12,6,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 6 note chords in level 0")
  expect(p, "plot is probably ok")
})
