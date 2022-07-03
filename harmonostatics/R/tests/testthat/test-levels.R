test_that("we can get the correct interval and level given an integer within, above or below the primary octave", {
  # level = -2
  expect_equal(level_and_interval_for(-25),c(level=2,interval=1))
  # level = -1
  expect_equal(level_and_interval_for(-24),c(level=1,interval=0))
  expect_equal(level_and_interval_for(-13),c(level=1,interval=1))
  # level = 0
  expect_equal(level_and_interval_for(-12),c(level=0,interval=12))
  expect_equal(level_and_interval_for(-1),c(level=0,interval=1))
  expect_equal(level_and_interval_for(0),c(level=0,interval=0))
  expect_equal(level_and_interval_for(1),c(level=0,interval=1))
  expect_equal(level_and_interval_for(12),c(level=0,interval=12))
  # level = +1
  expect_equal(level_and_interval_for(13),c(level=1,interval=1))
  expect_equal(level_and_interval_for(24),c(level=1,interval=12))
  # level = +2
  expect_equal(level_and_interval_for(25),c(level=2,interval=1))
  expect_equal(level_and_interval_for(36),c(level=2,interval=12))
  # level = +3
  expect_equal(level_and_interval_for(37),c(level=3,interval=1))
})

test_that("we can get the correct interval and level given a pair of integers within, above or below the primary octave", {
  expect_equal(level_and_interval_for(c(0,4)),c(level=0,interval=4))
  expect_equal(level_and_interval_for(c(0,7)),c(level=0,interval=7))
  expect_equal(level_and_interval_for(c(4,7)),c(level=0,interval=3))
  expect_equal(level_and_interval_for(c(-14,27)),c(level=3,interval=5))
  expect_equal(level_and_interval_for(c(5,-2)),c(level=0,interval=7))
  expect_equal(level_and_interval_for(c(7,14)),c(level=0,interval=7))
})
