test_that("level 0 brightness matches our expectations",{
  expect_equal(harmony.0.brightness_polarity(), c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1))
  expect_equal(harmony.0.brightness(), c( 0.12,-0.18,0.29,-2.00,2.00,-0.29,
                                          0.00,
                                          0.29,-2.00,2.00,-0.29,0.18,-0.12),tolerance=0.1)
})
test_that("brightness for chords matches our expectations",{

  expect_gt(brightness(c(0,4,7),0),0)
  expect_lt(brightness(c(0,3,7),0),0)

  # TODO: fix brightness so that these pass
  expect_lt(brightness(c(2,5,9),0),0)
  expect_gt(brightness(c(1,5,8),0),0)

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
test_that("brightness for intervals matches our expectations",{
  expect_gt(brightness(0,0),0)
  expect_gt(brightness(2,0),0)
  expect_gt(brightness(4,0),0)
  expect_gt(brightness(-8,0),0)
  expect_gt(brightness(7,0),0)
  expect_gt(brightness(9,0),0)
  expect_gt(brightness(-3,0),0)
  expect_gt(brightness(11,0),0)
  expect_equal(brightness(6,0),0)
  expect_lt(brightness(1,0),0)
  expect_lt(brightness(3,0),0)
  expect_lt(brightness(-9,0),0)
  expect_lt(brightness(5,0),0)
  expect_lt(brightness(8,0),0)
  expect_lt(brightness(-4,0),0)
  expect_lt(brightness(10,0),0)
  expect_lt(brightness(12,0),0)
})
