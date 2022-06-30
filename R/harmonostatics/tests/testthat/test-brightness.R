test_that("brightness for chords matches our expectations",{
  expect_gt(brightness(c(0,4,7),0),0)
  expect_gt(brightness(c(5,9,12),0),0)
  expect_gt(brightness(c(7,11,14),0),0)
  expect_gt(brightness(7,0),0)
  expect_gt(brightness(-5,0),0)
  expect_gt(brightness(-17,0),0)
})

test_that("triangular root brightness boundary works as expected", {
  expect_equal(harmony.0.brightness_boundary_triangular_root(),5)
})
