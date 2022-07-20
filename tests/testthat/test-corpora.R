test_that("hcorp is accessible within test env", {
  expect_equal(hcorp::classical_1b[[1]][[1]],hrep::pi_chord(c(43,59,62,67)))
  expect_equal(hcorp::classical_1b[[1]][[1]] %>% as.numeric,c(43,59,62,67))
})

test_that("we can plot hcorp lists", {
  title = "hcorp classical_1b-1 brightness affinity"
  home_note = 50
  chords = hcorp::classical_1b[[1]] %>% as.list %>% lapply(function(x) x-home_note)
  p = homey_plot_harmony(chords,home=0,columns=c("brightness","affinity"),title=title,include_names=FALSE,repel_labels=TRUE)
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".svg",sep="")))
  suppressMessages(ggplot2::ggsave(paste("./homey_plots/",gsub(" ", "_", title),".png",sep="")))
  expect_identical(p$labels$x, "brightness")
  expect_identical(p$labels$y, "affinity")
})
