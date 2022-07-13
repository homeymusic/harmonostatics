test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity_octave(),affinity_tonic())
  text(affinity_octave(),affinity_tonic(),labels=names(intervals_list()),pos=3)
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)

  # simple scatter plot
  p = plot(harmony_brightness_polarity(),harmony_affinity())
  text(harmony_brightness_polarity(),harmony_affinity(),labels=names(intervals_list()),pos=3)
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)

  title = "Tonic-Octave Affinity"
  tonic_octave_affinity = tibble::tibble(
    name = intervals()$name,
    tonic_affinity=affinity_tonic(),
    octave_affinity=affinity_octave()
  )
  p = tonic_octave_affinity %>% ggplot2::ggplot(ggplot2::aes(tonic_affinity,octave_affinity)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(title) +
    ggplot2::geom_label(ggplot2::aes(label=name),label.size = NA,fill=NA,vjust='bottom',hjust="outward",label.padding = ggplot2::unit(0.3, "lines")) +
    theme_homey()
  expect_identical(p$labels$x, "tonic_affinity")
  expect_identical(p$labels$y, "octave_affinity")
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
})

test_that("affinity brightness plots look good", {
  title = "Rotated Tonic-Octave Affinity"
  p = homey_plot_harmony(intervals_list(),home=0,columns=c("brightness_polarity","affinity"),title=title,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness_polarity")
  expect_identical(p$labels$y, "affinity")

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
  p = homey_plot_harmony(intervals_5_levels,home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE,include_names=FALSE,repel_labels=TRUE,x_expansion_mult=0.1)
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
test_that("all symmetrical eight note scales look good", {
  title="All Tonic-Octave Symmetrical 8-Note Scales"
  scales = utils::combn(1:11,6,function(x){c(0,x,12)},simplify=FALSE)
  p = plot_harmony(scales,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(scales,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=20)
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
test_that("PE of tonic intervals looks good", {
  title="Tonic PE of Intervals v Semitone"
  p = plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,x_expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")

  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,x_expansion_mult=0.2,title=title,include_names=FALSE)
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")


  title="Tonic PE of Intervals v Brightness"
  p = homey_plot_potential_energy(intervals_list(),y=0,home=0,unlist=TRUE,columns=c("brightness","potential_energy"),x_expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("PE of octave intervals looks good", {
  title="Octave PE of Intervals v Semitone"
  p = plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("semitone","potential_energy"),symmetrical=FALSE,x_expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")
  title="Octave PE of Intervals v Brightness"
  p = homey_plot_potential_energy(intervals_list(),y=12,home=12,unlist=TRUE,columns=c("brightness","potential_energy"),x_expansion_mult=0.2,title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("lydian PE look good", {
  title="Lydian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=lydian_tonic_chords(),y=lydian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=lydian_tonic_chords(),y=lydian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("ionian PE look good", {
  title="Ionian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Ionian Tonic Chords PE vs Semitone"
  p = plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=ionian_tonic_chords(),y=c(0,4,7),home=0,columns=c("semitone","potential_energy"), title=title, symmetrical=FALSE,x_expansion_mult=0.1,y_lim=100)
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
test_that("mixolydian PE look good", {
  title="Mixolydian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=mixolydian_tonic_chords(),y=mixolydian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=mixolydian_tonic_chords(),y=mixolydian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Mixolydian Tonic Sixth Chords PE vs Brightness"
  p = plot_potential_energy(x=mixolydian_tonic_sixth_chords(),y=mixolydian_tonic_sixth_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=mixolydian_tonic_sixth_chords(),y=mixolydian_tonic_sixth_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("dorian PE look good", {
  title="Dorian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=dorian_tonic_chords(),y=dorian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=dorian_tonic_chords(),y=dorian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Dorian Octave Chords PE vs Brightness"
  p = plot_potential_energy(x=dorian_octave_chords(),y=dorian_octave_chords()[1]%>%unlist,home=12,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=dorian_octave_chords(),y=dorian_octave_chords()[1]%>%unlist,home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  high_affinity_dorian_chords = c(-5,-3,-2,0,2,3,5,7,9,10,12,14,15,17) %>% utils::combn(3,simplify=FALSE,function(x){
    if (affinity(x)>=affinity(c(0,3,7))) {x} else {c(0,3,7)}
  }) %>% unique

  title="All Dorian Tonic Chords PE vs Brightness"
  p = homey_plot_potential_energy(x=high_affinity_dorian_chords,y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,include_names=FALSE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="All Dorian Octave Chords PE vs Brightness"
  p = homey_plot_potential_energy(x=high_affinity_dorian_chords,y=c(12,9,5),home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,include_names=FALSE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("aeolian PE look good", {
  title="Aeolian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Aeolian Tonic Chords PE vs Semitone"
  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"), title=title, symmetrical=FALSE,x_expansion_mult=0.1,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "semitone")
  expect_identical(p$labels$y, "potential_energy")

  p = plot_potential_energy(x=aeolian_tonic_chords(),y=c(0,3,7),home=0,columns=c("semitone","potential_energy"),title="ionian tonic chords")
  expect(p, "plot is probably ok")

  title="Aeolian Octave Chords PE vs Brightness"
  p = plot_potential_energy(x=aeolian_octave_chords(),y=aeolian_octave_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=aeolian_octave_chords(),y=aeolian_octave_chords()[1]%>%unlist,home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

})
test_that("phrygian chords look good", {
  title="Phrygian Tonic Chords PE"
  p = plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_tonic_chords(),y=c(0,3,7),home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Phrygian Octave Chords PE"
  p = plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=phrygian_octave_chords(),y=c(12,8,5),home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("locrian PE look good", {
  title="Locrian Tonic Chords PE vs Brightness"
  p = plot_potential_energy(x=locrian_tonic_chords(),y=locrian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=locrian_tonic_chords(),y=locrian_tonic_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")

  title="Locrian Octave Chords PE vs Brightness"
  p = plot_potential_energy(x=locrian_octave_chords(),y=locrian_octave_chords()[1]%>%unlist,home=0,columns=c("brightness","potential_energy"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_potential_energy(x=locrian_octave_chords(),y=locrian_octave_chords()[1]%>%unlist,home=12,columns=c("brightness","potential_energy"), title=title, symmetrical=TRUE,y_lim=100)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "potential_energy")
})

