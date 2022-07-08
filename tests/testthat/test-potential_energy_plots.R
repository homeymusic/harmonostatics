test_that("potential energy of tonic intervals looks good", {
  title="Tonic Potential Energy of Intervals v Semitone"
  p = plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")

  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,expansion_mult=0.2,title=title,include_names=FALSE)
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")


  title="Tonic Potential Energy of Intervals v Brightness"
  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("brightness","potential_energy"),expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("potential energy of octave intervals looks good", {
  title="Octave Potential Energy of Intervals v Semitone"
  p = plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")
  title="Octave Potential Energy of Intervals v Brightness"
  p = homey_plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("brightness","potential_energy"),expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("phrygian chords look good", {
  title="Phrygian Tonic Chords Potential Energy"
  p = plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Phrygian Octave Chords Potential Energy"
  p = plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("ionian potential energy look good", {
  title="Ionian Tonic Chords Potential Energy vs Brightness"
  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Ionian Tonic Chords Potential Energy vs Semitone"
  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"), title=title, symmetrical=FALSE,expansion_mult=0.1)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")

  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=ionian_tonic_chords.in_one_level(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
  p = plot_potential_energy(x=ionian_tonic_chords.in_one_level(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"),title="ionian tonic chords level 0")
  expect(p, "plot is probably ok")
})
test_that("aeolian potential energy look good", {
  title="Aeolian Tonic Chords Potential Energy vs Brightness"
  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Aeolian Tonic Chords Potential Energy vs Semitone"
  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"), title=title, symmetrical=FALSE,expansion_mult=0.1)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")

  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")
})
