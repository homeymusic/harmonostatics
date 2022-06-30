test_that("harmony.0 meets expectations", {
  expect_equal(harmony.0.brightness_polarity(), c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1))
  expect_equal(harmony.0.brightness(), c( 0.10,-0.25,0.50,-0.50,1.00,-0.20,
                                0.00,
                                0.20,-1.00,0.50,-0.50,0.25,-0.10))
  expect_equal(harmony.0.affinity(), c(15,1,3,7,6,10,3,10,6,7,3,1,15))
  # the mean of the tonic and octave affinities equals rotated affinity
  mean_affinity = rbind(affinity.0.octave(),affinity.0.tonic()) %>% colMeans
  expect_equal(harmony.0.affinity(),mean_affinity)
})

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
