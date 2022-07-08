potential_energy.uncached <- function(x,y,home,name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_character(name,null.ok=TRUE)

  tibble::tibble(
    semitone = x %>% mean,
    intervallic_name = x %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x),
    brightness=brightness(x,home),
    magnitude=magnitude(x,home),
    potential_energy = calculate_potential_energy(x,y,home)
  )
}
#' Potential Energy
#'
#' Provides the potential energy between two chords or notes
#'
#' @param x A note or chord expressed as an interval integers or vector of interval integers
#' @param y A note or chord expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param name An optional custom name for the note or chord
#' @return A tibble with semitone, intervallic_name, name, affinity, brightness, magnitude and potential energy
#'
#' @export
potential_energy <- memoise::memoise(potential_energy.uncached)

calculate_potential_energy.uncached <-function(x,y,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))

  crossing_of_pitches = tidyr::crossing(x=x,y=y) %>% dplyr::rowwise() %>%
    dplyr::mutate(potential_energy=calculate_potential_energy_for(x,y,home))

  crossing_of_pitches$potential_energy %>% mean
}
calculate_potential_energy <- memoise::memoise(calculate_potential_energy.uncached)

calculate_potential_energy_for.uncached <-function(x,y,home) {
  checkmate::qassert(x,"X1")
  checkmate::qassert(y,"X1")
  checkmate::assert_choice(home,c(0,12))

  x_magnitude = magnitude(x,home)
  y_magnitude = magnitude(y,home)
  abs(x_magnitude-y_magnitude)*(ifelse(home==0,x-y,y-x))

}
calculate_potential_energy_for <- memoise::memoise(calculate_potential_energy_for.uncached)
