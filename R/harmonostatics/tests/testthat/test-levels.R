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
