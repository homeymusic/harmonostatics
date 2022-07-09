##############
#
# all levels
#

harmony.uncached <- function(x, home=NULL, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12), null.ok=TRUE)
  if (is.null(home)) {
    if (0 %in% x) {
      home = 0
    } else if (12 %in% x) {
      home = 12
    } else {
      stop('home must be specified or x must contain 0 or 12 or both')
    }
  }
  tibble::tibble(
    semitone = x %>% mean,
    intervallic_name = x %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x),
    brightness=brightness(x,home),
    brightness_polarity=brightness_polarity(x,home),
    magnitude=harmony.magnitude(x,home),
  )
}

#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#' @param x A note or chord expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param name An optional custom name for the note or chord
#' @return A tibble with semitone, intervallic_name, name, affinity, brightness and harmony.magnitude
#'
#' @importFrom ggplot2 ggsave
#' @importFrom ggrepel geom_text_repel
#' @importFrom tidyr crossing
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

#' L2 norm of affinity and brightness
harmony.magnitude.uncached <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  sqrt(affinity(x)^2+brightness(x,home)^2)
}
harmony.magnitude <- memoise::memoise(harmony.magnitude.uncached)

#########
#
# level 0
#
# level 0 is the primary group of 13 semitones from tonic to octave
# we use the word level instead of octave throughout
# due to the name space collision between
# * octave: the name of an interval
# * octave: a group of a dozen semitones (level)

# rotate around the origin by the rotation angle
# changing the coordinate system
# from: octave-affinity versus tonic-affinity
# to: octave-tonic-affinity versus brightness-polarity
harmony.rotated_tonic_octave_affinity.uncached <-function() {
  # use the tritone to determine the rotation angle
  tritone = 6
  rotation_angle = atan2(affinity_octave()[tritone+1],affinity_tonic()[tritone+1])

  (affinity_tonic_octave() %>%
      rotate(rotation_angle) * cos(rotation_angle)) %>% zapsmall
}
harmony.rotated_tonic_octave_affinity <- memoise::memoise(harmony.rotated_tonic_octave_affinity.uncached)

harmony_brightness_polarity <- function() {
  harmony.rotated_tonic_octave_affinity()[1,]
}

harmony_affinity <- function() {
  harmony.rotated_tonic_octave_affinity()[2,]
}
