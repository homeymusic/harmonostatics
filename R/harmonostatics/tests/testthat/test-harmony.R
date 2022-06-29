test_that("harmony.0 meets expectations", {
  h = harmony.0()
  expect_equal(h$name %>% length,13)
  expect(tibble::is_tibble(h),"expected harmony to be a tibble")
  expect_equal(h$brightness_polarity, c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1))
  expect_equal(h$brightness, c( 0.10,-0.25,0.50,-0.50,1.00,-0.20,
                                0.00,
                                0.20,-1.00,0.50,-0.50,0.25,-0.10))
  expect_equal(h$affinity, c(15,1,3,7,6,10,3,10,6,7,3,1,15))
  # the mean of the tonic and octave affinities equals rotated affinity
  mean_affinity = rbind(affinity.0()$octave,affinity.0()$tonic) %>% colMeans
  expect_equal(h$affinity,mean_affinity)
  expected_tonic_gravity = c(0.00,1.03,6.08,21.05,24.33,50.00,
                             24.00,
                             70.01,48.66,63.16,30.41,11.33,180.00)
  expect_equal(h$tonic_gravity,expected_tonic_gravity,tolerance = TRUE)
  expect_equal(h$octave_gravity,expected_tonic_gravity %>% rev,tolerance = TRUE)
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

expected_tonic_affinity = c(16,0,4,6,7,9,3,11,5,8,2,2,14)
expected_octave_affinity = c(14,2,2,8,5,11,3,9,7,6,4,0,16)
expected_mean_affinity = c(15,1,3,7,6,10,3,10,6,7,3,1,15)

test_that("prime factor disaffinity matches expectations", {
  t_d = disaffinity.0()$tonic
  expect_equal(t_d,c(0,16,12,10,9,7,13,5,11,8,14,14,2))
  o_d = disaffinity.0()$octave
  expect_equal(o_d,c(2,14,14,8,11,5,13,7,9,10,12,16,0))
})

test_that("affinity form matches expectations", {
  a = affinity.0()
  expect(tibble::is_tibble(a),"expected affinity to be a tibble")
  expect_equal(a$tonic,expected_tonic_affinity)
  expect_equal(a$octave,expected_octave_affinity)
  mean_affinity = rbind(affinity.0()$octave,affinity.0()$tonic) %>% colMeans
  expect_equal(mean_affinity,expected_mean_affinity)
  expect_equal(mean_affinity,harmony.0()$affinity)
})

test_that("ET tritone disaffinity matches expectation", {
  expect_equal(et_tritone_disaffinity(),13)
})

test_that("affinity for chords matches our expectations",{
  expect_equal(affinity(c(0,4,7)),7.66,tolerance=TRUE)
  expect_equal(affinity(c(5,9,12)),7.66,tolerance=TRUE)
  expect_equal(affinity(c(7,11,14)),7.66,tolerance=TRUE)
  expect_equal(affinity(7),9,tolerance=TRUE)
  expect_equal(affinity(-5),8,tolerance=TRUE)
  expect_equal(affinity(-17),6,tolerance=TRUE)
})

test_that("brightness for chords matches our expectations",{
  expect_gt(brightness(c(0,4,7),0),0)
  expect_gt(brightness(c(5,9,12),0),0)
  expect_gt(brightness(c(7,11,14),0),0)
  expect_gt(brightness(7,0),0)
  expect_gt(brightness(-5,0),0)
  expect_gt(brightness(-17,0),0)
})
