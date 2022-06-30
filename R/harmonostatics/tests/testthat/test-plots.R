test_that("tonic.affinity octave-affinity scatter plots looks like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})

test_that("potential energy plots looks good", {
  h = -12:24 %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$semitone,h$potential_energy)
  expect_equal(p,NULL)
  p = plot(h$affinity,h$potential_energy)
  text(h$affinity,h$potential_energy,labels=h$intervallic_name)
  expect_equal(p,NULL)
})
