test_that("harmony for chords meets expectations", {
  expect_error(harmony(c(0,4,7)),"argument \"home\" is missing, with no default",fixed=TRUE)
  expect_error(harmony(0),"argument \"home\" is missing, with no default",fixed=TRUE)
  c_major_triad = harmony(c(0,4,7),0,"C Major Triad")
  expect(tibble::is_tibble(c_major_triad),"expected c major triad to be a tibble")
  expect_equal(c_major_triad$semitone,3.67,tolerance=TRUE)
  expect_equal(c_major_triad$intervallic_name,"0:4:7")
  expect_equal(c_major_triad$name,"C Major Triad")
  expect_equal(c_major_triad$affinity,7.666,tolerance=TRUE)
})

test_that("potential energy works as expected",{
  expect_equal(harmony(c(0,4,7),0)$potential_energy,28.16,tolerance=TRUE)
  expect_equal(harmony(c(5,9,12),0)$potential_energy,66.48,tolerance=TRUE)
  expect_equal(harmony(c(7,11,14),0)$potential_energy,81.83,tolerance=TRUE)
})
