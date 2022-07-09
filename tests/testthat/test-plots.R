test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  text(affinity.0.octave(),affinity.0.tonic(),labels=names(intervals_list()),pos=3)
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)

  # simple scatter plot
  p = plot(harmony.0.brightness_polarity(),harmony.0.affinity())
  text(harmony.0.brightness_polarity(),harmony.0.affinity(),labels=names(intervals_list()),pos=3)
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)

})

test_that("affinity brightness plots look good", {
  title = "Intervals"
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  p = homey_plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE,repel_labels=TRUE,include_names=TRUE)
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title="Tonic Intervals over 5 Levels"
  intervals_5_levels = list(-24:36)
  p = homey_plot_harmony(intervals_5_levels,home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE,include_names=FALSE,repel_labels=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title="Octave Intervals over 5 Levels"
  intervals_5_levels = list(-24:36)
  p = homey_plot_harmony(intervals_5_levels,home=12,unlist=TRUE,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE,include_names=FALSE,repel_labels=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title = "Common Scales"
  p = plot_harmony(common_scales(),home=0,columns=c("brightness","affinity"), title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales(),home=0,columns=c("brightness","affinity"),title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title = 'Locrian'
  p = plot_harmony(common_scales()['locrian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['locrian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title='Dorian'
  p = plot_harmony(common_scales()['dorian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['dorian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title='Lydian'
  p = plot_harmony(common_scales()['lydian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['lydian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

})

test_that("all one note pitches look good", {
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title="all 1 note pitches")
  expect(p, "plot is probably ok")
})
test_that("all two note chords look good", {
  p = plot_harmony(utils::combn(0:12,2,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 2 note chords tonic home")
  expect(p, "plot is probably ok")
  p = plot_harmony(utils::combn(0:12,2,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 2 note chords octave home")
  expect(p, "plot is probably ok")
})
test_that("all three note chords look good", {
  title="All 3 Note Chords"
  chords = c(utils::combn(1:11,2,function(x){c(0,x)},simplify=FALSE),utils::combn(1:11,2,function(x){c(x,12)},simplify=FALSE))
  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all symmetrical three note chords look good", {
  title="All Tonic-Octave Symmetrical 'Triads'"
  chords = utils::combn(1:11,2,function(x){c(0,x,12)},simplify=FALSE)
  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=20)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all four note chords look good", {
  title="All 4 Note Chords"
  chords = c(utils::combn(1:11,3,function(x){c(0,x)},simplify=FALSE),utils::combn(1:11,3,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=20)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all symmetrical four note chords look good", {
  title="All Tonic-Octave Symmetrical 'Tetrads'"
  chords = utils::combn(1:11,3,function(x){c(0,x,12)},simplify=FALSE)
  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=10)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all five note chords look good", {
  title="All 5 Note Chords"
  chords = c(utils::combn(1:11,4,function(x){c(0,x)},simplify=FALSE),utils::combn(1:11,4,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title = title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=30)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all six note chords look good", {
  title="All 6 Note Chords"
  chords = c(utils::combn(1:11,5,function(x){c(0,x)},simplify=FALSE),utils::combn(1:11,5,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=40)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
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
