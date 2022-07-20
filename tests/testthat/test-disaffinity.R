test_that("prime factor disaffinity matches expectations", {
  t_d = disaffinity_tonic()
  expect_equal(t_d,c(0,16,12,10,9,7,13,5,11,8,14,14,2))
  o_d = disaffinity_octave()
  expect_equal(o_d,c(2,14,14,8,11,5,13,7,9,10,12,16,0))
  p = plot(o_d,t_d,main="L1 Disaffinity Intervals")
  text(o_d,t_d,labels=names(intervals_list()),pos=2)
  expect_equal(p,NULL)
  expected_diff = rep(2,length(t_d))
  expected_diff[7] = 0
  expect_equal(abs(t_d - o_d),expected_diff)
})
test_that("ET tritone disaffinity matches expectation", {
  expect_equal(equal_temperament_tritone_disaffinity(),13)
})
test_that("prime factor l2 disaffinity matches expectations", {
  t_d = disaffinity_tonic(norm="l2")
  o_d = disaffinity_octave(norm="l2")
  p = plot(o_d,t_d,main="L2 Disaffinity Intervals")
  text(o_d,t_d,labels=names(intervals_list()),pos=2)
  expect_equal(p,NULL)
})
