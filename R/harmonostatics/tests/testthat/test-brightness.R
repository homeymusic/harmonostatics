test_that("level 0 brightness matches our expectations",{
  expect_equal(harmony.0.brightness_polarity(), c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1))
  expect_equal(harmony.0.brightness(), c( 0.12,-0.18,0.29,-2.00,2.00,-0.29,
                                        0.00,
                                        0.29,-2.00,2.00,-0.29,0.18,-0.12),tolerance=TRUE)
})
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
test_that("the mean affinity of the minor/major 3rd and minor/major 6th are as expected", {
  expect_equal(harmony.0.brightness_boundary_3rds_and_6ths(),6.5)
})
