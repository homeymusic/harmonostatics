test_that("prime factors match expectations", {
  t_d = tonic.disaffinity()
  expect_equal(t_d,c(0,16,12,10,9,7,12,5,11,8,14,14,2))

  o_d = octave.disaffinity()
  expect_equal(o_d,c(2,14,14,8,11,5,14,7,9,10,12,16,0))
})
