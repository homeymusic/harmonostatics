test_that("harmony meets expectaions", {
  h = harmony()
  expect_equal(h$name %>% length,13)
  expect(tibble::is_tibble(h),"expected harmony to be a tibble")
  expect_equal(h$polarity, c(1,-1,1,-1,1,-1,0,1,-1,1,-1,1,-1))
  expect_equal(h$brightness, c( 0.10,-0.25,0.50,-0.50,1.00,-0.20,
                                0.00,
                                0.20,-1.00,0.50,-0.50,0.25,-0.10))
  expect_equal(h$affinity, c(15,1,3,7,6,10,4,10,6,7,3,1,15))
  # the mean of the tonic and octave affinities equals rotated affinity
  expect_equal(h$affinity,affinity()$mean)
})
