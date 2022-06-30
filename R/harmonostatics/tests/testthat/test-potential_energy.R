test_that("multiplication works", {
  affinity = 9
  brightness = 0.5
  home = 0
  semitone = 7
  expect_equal(potential_energy(affinity,brightness,home,semitone),63.10,tolerance=TRUE)
})
