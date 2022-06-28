test_that("prime factor disaffinity matches expectations", {
  t_d = tonic.disaffinity()
  expect_equal(t_d,c(0,16,12,10,9,7,12,5,11,8,14,14,2))

  o_d = octave.disaffinity()
  expect_equal(o_d,c(2,14,14,8,11,5,12,7,9,10,12,16,0))
})

test_that("prime factor affinity matches expectations", {
  t_a = tonic.affinity()
  expect_equal(t_a,c(16,0,4,6,7,9,4,11,5,8,2,2,14))

  o_a = octave.affinity()
  expect_equal(o_a,c(14,2,2,8,5,11,4,9,7,6,4,0,16))
})
