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
  title = "Harmony.0 Field"
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title=title,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
  p = plot_harmony(list("level:+1"=12:24),unlist=TRUE,home=0,columns=c("brightness","affinity"),include_names=FALSE)
  expect(p, "plot is probably ok")

  title = "Common Scales"
  p = plot_harmony(common_scales(),home=0,columns=c("brightness","affinity"), title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales(),home=0,columns=c("brightness","affinity"),title=title)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title = 'Locrian'
  p = plot_harmony(common_scales()['locrian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['locrian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title='Dorian'
  p = plot_harmony(common_scales()['dorian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['dorian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

  title='Lydian'
  p = plot_harmony(common_scales()['lydian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(common_scales()['lydian'],home=0,unlist=TRUE,columns=c("brightness","affinity"),title=title,include_names=FALSE,pascal_triangle=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")

})

test_that("all one note pitches look good", {
  p = plot_harmony(intervals_list(),home=0,columns=c("brightness","affinity"),title="all 1 note pitches")
  expect(p, "plot is probably ok")
})
test_that("all two note chords look good", {
  p = plot_harmony(combn(0:12,2,simplify=FALSE),home=0,columns=c("brightness","affinity"),title="all 2 note chords tonic home")
  expect(p, "plot is probably ok")
  p = plot_harmony(combn(0:12,2,simplify=FALSE),home=12,columns=c("brightness","affinity"),title="all 2 note chords octave home")
  expect(p, "plot is probably ok")
})
test_that("all three note chords look good", {
  title="All 3 Note Chords"
  chords = c(combn(1:11,2,function(x){c(0,x)},simplify=FALSE),combn(1:11,2,function(x){c(x,12)},simplify=FALSE))
  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")
  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all four note chords look good", {
  title="All 4 Note Chords"
  chords = c(combn(1:11,3,function(x){c(0,x)},simplify=FALSE),combn(1:11,3,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=20)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all five note chords look good", {
  title="All 5 Note Chords"
  chords = c(combn(1:11,4,function(x){c(0,x)},simplify=FALSE),combn(1:11,4,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title = title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=30)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
test_that("all six note chords look good", {
  title="All 6 Note Chords"
  chords = c(combn(1:11,5,function(x){c(0,x)},simplify=FALSE),combn(1:11,5,function(x){c(x,12)},simplify=FALSE))

  p = plot_harmony(chords,columns=c("brightness","affinity"),title=title)
  expect(p, "plot is probably ok")

  p = homey_plot_harmony(chords,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE,max_overlaps=40)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",title,".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
