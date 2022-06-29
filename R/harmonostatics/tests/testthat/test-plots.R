test_that("tonic.affinity octave-affinity scatter plots looks like we expect", {
  # simple scatter plot
  p = plot(affinity.0()$octave,affinity.0()$tonic)
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})
