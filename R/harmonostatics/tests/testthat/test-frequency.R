test_that("frequency is structured as we expect", {
  t = tonic.frequency()
  o = octave.frequency()
  expect(tibble::is_tibble(t),"expected tonic frequency ratios to be a tibble")
  expect(tibble::is_tibble(o),"expected octave frequency ratios to be a tibble")
  expect_length(t$interval,13)
  expect_length(o$interval,13)
  expect_equal(t$numerator,c(1,16,9,6,5,4,7,3,8,5,16,15,2))
  expect_equal(t$denominator,c(1,15,8,5,4,3,5,2,5,3,9,8,1))
  expect_equal(o$numerator,rev(c(1,15,8,5,4,3,5,2,5,3,9,8,1)))
  expect_equal(o$denominator,rev(c(1,16,9,6,5,4,7,3,8,5,16,15,2)))
  expect_equal(t$name,c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
                        "perfect 4th","tritone","perfect 5th","minor 6th",
                        "major 6th","minor 7th","major 7th","octave"))
  expect_equal(o$name,c("tonic","minor 2nd","major 2nd","minor 3rd","major 3rd",
                        "perfect 4th","tritone","perfect 5th","minor 6th",
                        "major 6th","minor 7th","major 7th","octave"))
  expect_equal(t$ratio,c("1:1","16:15","9:8","6:5","5:4","4:3",
                                  "7:5",
                                  "3:2","8:5","5:3","16:9","15:8","2:1"))
  expect_equal(o$ratio,c("1:2","8:15","9:16","3:5","5:8","2:3",
                               "5:7",
                               "3:4","4:5","5:6","8:9","15:16","1:1"))
})
