expected_tonic_affinity = c(16,0,4,6,7,9,3,11,5,8,2,2,14)
expected_octave_affinity = c(14,2,2,8,5,11,3,9,7,6,4,0,16)
expected_mean_affinity = c(15,1,3,7,6,10,3,10,6,7,3,1,15)

test_that("affinity at level 0 meets expectations", {
  expect_equal(harmony.0.affinity(), c(15,1,3,7,6,10,3,10,6,7,3,1,15))
  # the mean of the tonic and octave affinities equals rotated affinity
  mean_affinity = rbind(affinity.0.octave(),affinity.0.tonic()) %>% colMeans
  expect_equal(harmony.0.affinity(),mean_affinity)
})

test_that("affinity form matches expectations", {
  t = affinity.0.tonic()
  o = affinity.0.octave()
  expect_equal(t,expected_tonic_affinity)
  expect_equal(o,expected_octave_affinity)
  mean_affinity = rbind(o,t) %>% colMeans
  expect_equal(mean_affinity,expected_mean_affinity)
  expect_equal(mean_affinity,harmony.0.affinity())
})

test_that("affinity for chords matches our expectations",{
  expect_equal(affinity(c(0,4,7)),7.66,tolerance=TRUE)
  expect_equal(affinity(c(5,9,12)),7.66,tolerance=TRUE)
  expect_equal(affinity(c(7,11,14)),7.66,tolerance=TRUE)
  expect_equal(affinity(7),9,tolerance=TRUE)
  expect_equal(affinity(-5),8,tolerance=TRUE)
  expect_equal(affinity(-17),6,tolerance=TRUE)
})
