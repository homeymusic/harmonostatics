test_that("potential energy of triads in harmony context works as expected",{
  expect_equal(potential_energy(x=c(0,3,7),y=c(0,3,7),home=0)$potential_energy,0)
  expect_equal(potential_energy(x=c(0,4,7),y=c(0,4,7),home=0)$potential_energy,0)
  expect_equal(potential_energy(x=c(7,11,14),y=c(0,4,7),home=0)$potential_energy,61.53,tolerance=.001)
})
