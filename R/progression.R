progression <- function(from,to,name=NULL) {
  checkmate::assert_tibble(from, ncols=8, nrows=1, any.missing = FALSE)
  checkmate::assert_tibble(to, ncols=8, nrows=1, any.missing = FALSE)
  if (from$home != to$home) {
    stop('from$home and to$home must have the same value.')
  }
  intervallic_name = paste(from$intervallic_name," \U2192 ", to$intervallic_name)
  if (is.null(name)) {
    name = intervallic_name
  }
  tibble::tibble(
    name = name,
    home = from$home,
    intervallic_name = intervallic_name,
    semitone_difference = from$semitone - to$semitone,
    from_affinity = from$affinity,
    to_affinity = to$affinity,
    affinity_difference = from$affinity - to$affinity,
    brightness_difference = from$brightness - to$brightness,
    potential_energy_difference = potential_energy_difference(attr(from,"pitches"),attr(to,"pitches"),from$home)
  )
}
