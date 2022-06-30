test_that("potential energy matches expectations", {
  affinity = 9
  brightness = 0.5
  home = 0
  semitone = 7
  expect_equal(potential_energy(affinity,brightness,home,semitone),63.10,tolerance=TRUE)
})
test_that("potential energy in harmony context works as expected",{
  expect_equal(harmony(12,0)$potential_energy,2.82,tolerance=TRUE)
  expect_equal(harmony(0,0)$potential_energy,0)
  expect_equal(harmony(5,0)$potential_energy,25,tolerance=TRUE)
  expect_equal(harmony(7,0)$potential_energy,35,tolerance=TRUE)
  expect_equal(harmony(12,12)$potential_energy,0)
  expect_equal(harmony(0,12)$potential_energy,2.82,tolerance=TRUE)
  expect_equal(harmony(5,12)$potential_energy,35,tolerance=TRUE)
  expect_equal(harmony(7,12)$potential_energy,25,tolerance=TRUE)
})
