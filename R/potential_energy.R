potential_energy.uncached <- function(x,y,home,name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_character(name,null.ok=TRUE)

  tibble::tibble(
    semitone_x = x %>% mean,
    semitone_y = y %>% mean,
    intervallic_name_x = x %>% paste(collapse = ":"),
    intervallic_name_y = y %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x),
    brightness=brightness(x,home),
    magnitude=harmony.magnitude(x,home),
    potential_energy = potential_energy_difference(x,y,home)
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

potential_energy_difference.uncached <-function(x,y,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_integerish(y)
  checkmate::assert_choice(home,c(0,12))

  crossing_of_pitches = tidyr::crossing(x,y) %>% dplyr::rowwise() %>%
    dplyr::mutate(potential_energy=
      abs(harmony.magnitude(x,home)-harmony.magnitude(y,home))*(ifelse(home==0,x-y,y-x)))

  crossing_of_pitches$potential_energy %>% mean
}
potential_energy_difference <- memoise::memoise(potential_energy_difference.uncached)
