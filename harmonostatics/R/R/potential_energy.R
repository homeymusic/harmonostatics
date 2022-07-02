potential_energy.uncached <- function(x,home,home_chord) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_integerish(home_chord)

  t = tidyr::crossing(x=x,y=home_chord) %>% dplyr::rowwise() %>%
    dplyr::mutate(potential_energy=calculate_potential_energy(x,y,home))

  t$potential_energy %>% mean
}
potential_energy <- memoise::memoise(potential_energy.uncached)

calculate_potential_energy.uncached <-function(x,y,home) {
  checkmate::qassert(x,"X1")
  checkmate::qassert(y,"X1")
  checkmate::assert_choice(home,c(0,12))

  x_magnitude = magnitude(x,home)
  y_magnitude = magnitude(y,home)

  abs(x_magnitude-y_magnitude)*(ifelse(home==0,x-y,y-x))
}
calculate_potential_energy <- memoise::memoise(calculate_potential_energy.uncached)
