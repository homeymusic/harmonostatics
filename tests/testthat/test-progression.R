test_that("home chord to home chord cadence makes sense", {
  from = c(0,4,7)
  to = c(0,4,7)
  p = progression(from,to,0)
  # must be a harmony tibble
  expect(tibble::is_tibble(p),"expected progression to be a tibble")
  expect_equal(p$intervallic_name,"0:4:7")
  expect_equal(p$name,p$intervallic_name)
  expect_equal(p$home,0)
  expect_equal(p$semitone,0)
  expect_equal(p$affinity,0)
  expect_equal(p$brightness,0)
  expect_equal(p$potential_energy,0)
})
test_that("tonic major dominant cadence makes sense", {
  from = c(7,11,14)
  to = c(0,4,7)
  p = progression(from,to,0)
  # must be a harmony tibble
  expect(tibble::is_tibble(p),"expected progression to be a tibble")
  expect_equal(p$intervallic_name,"7:11:14")
  expect_equal(p$name,p$intervallic_name)
  expect_equal(p$home,0)
  expect_equal(p$semitone,-7)
  expect_equal(p$from_semitone,harmony(from,0)$semitone)
  expect_equal(p$to_semitone,harmony(to,0)$semitone)
  expect_equal(p$from_affinity,harmony(from,0)$affinity)
  expect_equal(p$to_affinity,harmony(to,0)$affinity)
  expect_equal(p$from_brightness,harmony(from,0)$brightness)
  expect_equal(p$to_brightness,harmony(to,0)$brightness)
  expect_equal(p$affinity,0)
  expect_equal(p$brightness,0)
  expect_equal(p$potential_energy,61.53832,tolerance=0.01)
})
