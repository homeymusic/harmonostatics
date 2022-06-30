common_scales <- function() {
  list("lydian"=c(0,2,4,6,7,9,11,12),
       "ionian"=c(0,2,4,5,7,9,11,12),
       "mixolydian"=c(0,2,4,5,7,9,10,12),
       "dorian"=c(0,2,3,5,7,9,10,12),
       "aeolian"=c(0,2,3,5,7,8,10,12),
       "phrygian"=c(0,1,3,5,7,8,10,12),
       "locrian"=c(0,1,3,5,6,8,10,12),
       "chromatic"=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
       "major pentatonic"=c(0,2,4,7,9,12),
       "blues major pentatonic"=c(0,2,5,7,9,12),
       "suspended pentatonic"=c(0,2,5,7,10,12),
       "minor pentatonic"=c(0,3,5,7,10,12),
       "blues minor pentatonic"=c(0,3,5,8,10,12))
}

test_that("tonic.affinity octave-affinity scatter plots look like we expect", {
  # simple scatter plot
  p = plot(affinity.0.octave(),affinity.0.tonic())
  # do not see how to test much with the default scatter plot
  expect_equal(p,NULL)
})

test_that("potential energy plots look good", {
  h = -12:24 %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$semitone,h$potential_energy)
  expect_equal(p,NULL)
  p = plot(h$affinity,h$potential_energy)
  text(h$affinity,h$potential_energy,labels=h$intervallic_name)
  expect_equal(p,NULL)
})

test_that("affinity brightness plots look good", {
  h = -12:0 %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = 0:12 %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = 12:24 %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = common_scales()['locrian'] %>% unlist %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = common_scales()['dorian'] %>% unlist %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = common_scales()['lydian'] %>% unlist %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
  h = common_scales() %>% purrr::map(harmony,0) %>% purrr::map_dfr(.f=dplyr::bind_rows)
  p = plot(h$brightness,h$affinity)
  t = text(h$brightness,h$affinity,labels=h$intervallic_name,pos=1)
  expect_equal(p,NULL)
  expect_equal(t,NULL)
})

