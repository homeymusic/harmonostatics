# from Generalizing Musical Intervals by Dmitri Tymoczko
test_that("the first few progressions give reasonable results", {
  # assume the home note is G = 12
  p_g4_to_ef4 = progression(8,12,12)
  expect_equal(p_g4_to_ef4$potential_energy,34.70,tolerance=0.001)
  expect_equal(p_g4_to_ef4$brightness,1.882,tolerance=0.001)
  p_f4_to_df4 = progression(6,10,12)
  expect_equal(p_f4_to_df4$potential_energy,0.05429892,tolerance=0.001)
  expect_equal(p_f4_to_df4$brightness,-0.286,tolerance=0.001)
  p_b_to_g = progression(16,12,12)
  expect_equal(p_b_to_g$potential_energy,39.8,tolerance=0.001)
  expect_equal(p_b_to_g$brightness,-0.784,tolerance=0.001)
  p_g4_to_ef5 = progression(8,0,0)
  expect_equal(p_g4_to_ef5$potential_energy,69.40,tolerance=0.001)
  expect_equal(p_g4_to_ef5$brightness,2.118,tolerance=0.001)
  p_g4_to_ef4 = progression(8,12,12)
  expect_equal(p_g4_to_ef4$potential_energy,34.70,tolerance=0.001)
  expect_equal(p_g4_to_ef4$brightness,1.882,tolerance=0.001)
  # assume the home note is B = 0
  bf_to_ce = progression(c(1,5),c(0,6),0)
  expect_equal(bf_to_ce$potential_energy,5.51,tolerance=0.001)
  expect_equal(bf_to_ce$brightness,-1,tolerance=0.001)
  cfs_to_dff = progression(c(4,6),c(1,7),0)
  expect_equal(cfs_to_dff$potential_energy,1.9499,tolerance=0.001)
  expect_equal(cfs_to_dff$brightness,-0.143,tolerance=0.001)
})
test_that("the dyad matrix from fig. 3a give reasonable results", {
  home = 0
  to = c(0,6)
  from = utils::combn(0:12,2,simplify=FALSE)
  title = "PE of All Dyads in One Octave"
  p = homey_plot_progression(x=from,y=to,home=home,columns=c(x="from_affinity",y="potential_energy",size="from_brightness_mag",color="from_brightness"), title=title, symmetrical=FALSE,include_names=FALSE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "from_affinity")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("Fig 1A 18th Century", {
  home = 0
  to = c(0,4,7)
  from =list("1"=c(0,4,7),
             "2"=c(0,5,9),
             "3"=c(0,4,7),
             "4"=c(-1,2,7),
             "5"=c(0,4,7))
  title = "Fig 1A 18th Century"
  p = homey_plot_progression(x=from,y=to,home=home,columns=c(x="from_affinity",y="potential_energy",size="from_brightness_mag",color="from_brightness"), title=title, symmetrical=FALSE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "from_affinity")
  expect_identical(p$labels$y, "potential_energy")
})
test_that("common jazz-piano voice-leading pattern", {
  home = 0
  to = c(0,5,6,10)
  from =list("1"=c(0,5,6,10),
             "2"=c(-1,3,6,10),
             "3"=c(-2,3,5,8),
             "4"=c(-3,1,3,8))
  title = "Fig 1B Jazz"
  p = homey_plot_progression(x=from,y=to,home=home,columns=c(x="from_affinity",y="potential_energy",size="from_brightness_mag",color="from_brightness"), title=title, symmetrical=FALSE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "from_affinity")
  expect_identical(p$labels$y, "potential_energy")
})
