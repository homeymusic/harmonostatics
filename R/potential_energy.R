potential_energy.uncached <-function(from,to,home) {
  checkmate::assert_integerish(from)
  checkmate::assert_integerish(to)
  checkmate::assert_choice(home,c(0,12))

  crossing_of_pitches = tidyr::crossing(from,to) %>% dplyr::rowwise() %>%
    dplyr::mutate(potential_energy=
      abs(harmony.magnitude(from,home)-harmony.magnitude(to,home))*(ifelse(home==0,from-to,to-from)))

  crossing_of_pitches$potential_energy %>% mean
}
#' Potential Energy
#'
#' Provides the potential energy between two chords or notes
#'
#' @param from A starting note or chord expressed as a harmony tibble
#' @param to An ending note or chord expressed as a harmony tibble
#' @param home The home note of the musical context
#' @return The potential energy difference between the two chords or notes
#'
#' @export
potential_energy <- memoise::memoise(potential_energy.uncached)
