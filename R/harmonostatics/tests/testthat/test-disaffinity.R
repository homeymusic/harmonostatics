test_that("prime factor disaffinity matches expectations", {
  t_d = disaffinity.0.tonic()
  expect_equal(t_d,c(0,16,12,10,9,7,13,5,11,8,14,14,2))
  o_d = disaffinity.0.octave()
  expect_equal(o_d,c(2,14,14,8,11,5,13,7,9,10,12,16,0))
})

test_that("ET tritone disaffinity matches expectation", {
  expect_equal(equal_temperament_tritone_disaffinity(),13)
})

