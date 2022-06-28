test_that("rotation works", {
  angle = pi/4
  expect_error(rotate(cbind(1,0),angle))
  expect_equal(rotate(rbind(1,0),angle),rbind(cos(angle),sin(angle)))
  expect_equal(rotate(rbind(0,1),angle),rbind(-sin(angle),cos(angle)))
})
