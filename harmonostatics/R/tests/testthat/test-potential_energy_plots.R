test_that("phrygian chords look good", {
  title="Phrygian Tonic Chords Potential Energy"
  p = plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Phrygian Octave Chords Potential Energy"
  p = plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"), title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("potential energy look good", {
  title="Ionian Tonic Chords Potential Energy"
  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"), title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=list("level:0"=0:12),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),title="Tonic Potential Energy 0",include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=list("level:0"=0:12),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),title="Octave Potential Energy 0",include_names=FALSE)
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=ionian_tonic_chords.in_one_level(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=ionian_tonic_chords.in_one_level(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
})
