test_that("rotation works", {
  angle = pi/4
  expect_error(rotate(cbind(1,0),angle))
  expect_equal(rotate(rbind(1,0),angle),rbind(cos(angle),sin(angle)))
  expect_equal(rotate(rbind(0,1),angle),rbind(-sin(angle),cos(angle)))
})

test_that("sum_of_prime_factors works", {
  expect_equal(sum_of_prime_factors(numbers::primeFactors(1)),0)
  expect_equal(sum_of_prime_factors(numbers::primeFactors(16)),8)
})

test_that("triangular_root works", {
  expect_equal(triangular_root(15),5)
})
