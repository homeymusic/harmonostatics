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
  tibble(
    semitone = x %>% mean,
    intervallic_name = x %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x,home),
    brightness=brightness(x,home),
    magnitude=magnitude(x,home),
  )
}
#' Harmony
#'
#' Provides the musical harmony metrics of a note or chord.
#'
#' @param x A note or chord expressed as an interval integers or vector of interval integers
#' @param home The home pitch expressed an as interval integer
#' @param name=NULL An optional custom name for the note or chord
#' @return A tibble with semitone, intervallic_name, name, affinity, brightness and magnitude
#'
#' @examples
#' harmony(4,0) # provides the harmony metrics of the Major 3rd
#' harmony(8,12) # provides the harmony metrics of the inverted Major 3rd (Minor 6th with Octave)
#'
#' harmony(x=c(0,4,7),home=0, name="C Major Triad") # provides the harmony metrics of a C Major Triad
#' harmony(x=c(12,8,5),home=12, name="Inverted C Major Triad") # provides the harmony metrics of the inverted C Major Triad
#'
#' @export
harmony <- memoise::memoise(harmony.uncached)

magnitude.orig <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  sqrt(affinity(x,home)^2+brightness(x,home)^2)
}
magnitude <- memoise::memoise(magnitude.orig)

#########
#
# level 0
#

# level 0 is the primary group of 13 semitones from tonic to octave
# we use the word level instead of octave throughout
# due to the name space collision between
# * octave as in the interval name
# * octave as in the group of 13 semitones (level)

# Directional Derivative:
# rotate around the origin by the rotation angle
# changing the coordinate system
# from: octave-affinity versus tonic-affinity
# to: octave-tonic-affinity versus brightness-polarity
harmony.0.rotated_octave_affinity_tonic_affinity.uncached <-function() {
  # use the tritone to determine the rotation angle
  tritone_i = 6 + 1
  rotation_angle = atan2(affinity.0.octave()[tritone_i],affinity.0.tonic()[tritone_i])

  (rbind(affinity.0.tonic(),affinity.0.octave()) %>%
      rotate(rotation_angle) * cos(rotation_angle)) %>% zapsmall
}
harmony.0.rotated_octave_affinity_tonic_affinity <- memoise::memoise(harmony.0.rotated_octave_affinity_tonic_affinity.uncached)
