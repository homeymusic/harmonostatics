expected_tonic_affinity = c(16,0,4,6,7,9,4,11,5,8,2,2,14)
expected_octave_affinity = c(14,2,2,8,5,11,4,9,7,6,4,0,16)
expected_mean_affinity = c(15,1,3,7,6,10,4,10,6,7,3,1,15)

test_that("prime factor disaffinity matches expectations", {
  t_d = tonic.disaffinity()
  expect_equal(t_d,c(0,16,12,10,9,7,12,5,11,8,14,14,2))
  o_d = octave.disaffinity()
  expect_equal(o_d,c(2,14,14,8,11,5,12,7,9,10,12,16,0))
})

test_that("affinity form matches expectations", {
  a = affinity()
  expect(tibble::is_tibble(a),"expected affinity to be a tibble")
  expect_equal(a$tonic,expected_tonic_affinity)
  expect_equal(a$octave,expected_octave_affinity)
  expect_equal(a$mean,expected_mean_affinity)
  expect_equal(a$mean,harmony()$affinity)
})
