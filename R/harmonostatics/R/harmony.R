harmony <- function(x, home, name=NULL) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  tibble(
    semitone = x %>% mean,
    intervallic_name = x %>% paste(collapse = ":"),
    name = name,
    affinity=affinity(x),
    brightness=brightness(x,home)
  )
}

# level 0 is the primary group of 13 semitones from tonic to octave
# we use the word level instead of octave throughout
# because the name space collision between
# * octave as in the interval name
# * octave as in the group of 13 semitones (level)

harmony.0.rotation_angle <- function() {
  # use the tritone to determine the rotation angle
  tritone_i = 6 + 1
  atan2(affinity.0.octave()[tritone_i],affinity.0.tonic()[tritone_i])
}
harmony.0.affinity_brightness_polarity <-function() {
  # Directional Derivative:
  # rotate around the origin by the rotation angle
  # changing the coordinate system
  # from: octave-affinity versus tonic-affinity
  # to: octave-tonic-affinity versus brightness-polarity
  (rbind(affinity.0.tonic(),affinity.0.octave()) %>%
     rotate(harmony.0.rotation_angle()) * cos(harmony.0.rotation_angle())) %>% zapsmall
}
