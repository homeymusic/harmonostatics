progression.uncached <- function(from,to,home=NULL,name=NULL) {
  from = harmony(from,home)
  to = harmony(to,home)
  checkmate::assert_tibble(from, ncols=8, nrows=1, any.missing = FALSE)
  checkmate::assert_tibble(to, ncols=8, nrows=1, any.missing = FALSE)
  if (from$home != to$home) {
    stop('from$home and to$home must have the same value.')
  }
  intervallic_name = paste(from$intervallic_name," ", to$intervallic_name)
  if (is.null(name)) {
    name = intervallic_name
  }
  tibble::tibble(
    name = name,
    home = from$home,
    intervallic_name = intervallic_name,
    from_semitone = from$semitone,
    to_semitone = to$semitone,
    from_affinity = from$affinity,
    to_affinity = to$affinity,
    from_brightness = from$brightness,
    to_brightness = to$brightness,
    semitone = to$semitone - from$semitone,
    affinity = to$affinity - from$affinity,
    brightness = to$brightness - from$brightness,
    potential_energy = potential_energy(attr(from,"pitches"),attr(to,"pitches"),from$home)
  )
}
#' Progression
#'
#' Provides information about a progression between two chords or notes
#'
#' @param from A starting note or chord expressed as a harmony tibble
#' @param to An ending note or chord expressed as a harmony tibble
#' @param home The home note of the current musical context
#' @param name A name to describe the progression
#' @return A tibble with information about the change in harmony from to
#'
#' @export
progression <- memoise::memoise(progression.uncached)
