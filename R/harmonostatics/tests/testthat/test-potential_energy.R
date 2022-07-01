test_that("potential energy in harmony context works as expected",{
  expect_equal(potential_energy(c(0,4,7),0),23.23,tolerance=.001)
  expect_equal(potential_energy(c(0,4,7),0,c(0,4,7)),0)
  expect_equal(potential_energy(c(7,11,14),0,c(0,4,7)),61.53,tolerance=.001)
})

test_that("potential energy in harmony context works as expected",{
  expect_equal(harmony(12,0)$potential_energy,0,tolerance=.001)
  expect_equal(harmony(0,0)$potential_energy,0,tolerance=.001)
  expect_equal(harmony(5,0)$potential_energy,25,tolerance=.001)
  expect_equal(harmony(7,0)$potential_energy,35,tolerance=.001)
  expect_equal(harmony(12,12)$potential_energy,0,tolerance=.001)
  expect_equal(harmony(0,12)$potential_energy,0,tolerance=.001)
  expect_equal(harmony(5,12)$potential_energy,35,tolerance=.001)
  expect_equal(harmony(7,12)$potential_energy,25,tolerance=.001)
})
