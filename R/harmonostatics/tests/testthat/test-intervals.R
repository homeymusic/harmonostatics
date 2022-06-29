test_that("intervals match expected form", {
  i = intervals.0()
  expect_equal(i$semitone,0:12)
  expect_equal(i$name,c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
                        "perfect 4th","tritone","perfect 5th","minor 6th",
                        "major 6th","minor 7th","major 7th","octave"))

})
