test_that("we can get the correct interval and level given an integer within, above or below the primary octave", {
  # level = -1
  expect_equal(level_and_interval_of(-13),c(level=-2,interval=11))
  expect_equal(level_and_interval_of(-12),c(level=-1,interval=0))
  expect_equal(level_and_interval_of(-1),c(level=-1,interval=11))
  # level = 0
  expect_equal(level_and_interval_of(0),c(level=0,interval=0))
  expect_equal(level_and_interval_of(12),c(level=0,interval=12))
  # level = +1
  expect_equal(level_and_interval_of(13),c(level=1,interval=1))
  expect_equal(level_and_interval_of(24),c(level=1,interval=12))
  expect_equal(level_and_interval_of(25),c(level=2,interval=1))
})

test_that("we can get the correct interval and level given a pair of integers within, above or below the primary octave", {
  expect_equal(level_and_interval_of(c(0,4)),c(level=0,interval=4))
  expect_equal(level_and_interval_of(c(0,7)),c(level=0,interval=7))
  expect_equal(level_and_interval_of(c(4,7)),c(level=0,interval=3))
  expect_equal(level_and_interval_of(c(-14,27)),c(level=1,interval=1))
})
