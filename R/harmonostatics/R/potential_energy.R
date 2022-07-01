# TODO: need to require the home_chord even if it's just the same as home for single notes
potential_energy <- function(x,home,home_chord=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_integerish(home_chord,null.ok = TRUE)

  if (home_chord %>% is.null) {
    sapply(x,calculate_potential_energy,home,home) %>% mean
  } else {
    t = tidyr::crossing(x=x,y=home_chord) %>% dplyr::rowwise() %>% dplyr::mutate(potential_energy=calculate_potential_energy(x,y,home))
    t$potential_energy %>% mean
  }
}
calculate_potential_energy <-function(x,y,home) {
  checkmate::qassert(x,"X1")
  checkmate::qassert(y,"X1")
  checkmate::assert_choice(home,c(0,12))

  x_magnitude = magnitude(x,home)
  y_magnitude = magnitude(y,home)



  # abs(x_magnitude-y_magnitude)*abs(x-y)
  abs(x_magnitude-y_magnitude)*(ifelse(home==0,x-y,y-x))
}
