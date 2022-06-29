harmony <- function() {
  affinity_tonic = affinity()$tonic
  affinity_octave = affinity()$octave

  # use the tritone to determine the rotation angle
  tritone_i = 6 + 1
  angle = atan2(affinity_octave[tritone_i],affinity_tonic[tritone_i])

  # Directional Derivative:
  #
  # rotate around the origin by the rotation angle
  # we will change coordinate systems
  # from: octave-affinity versus tonic-affinity
  # to: octave-tonic-affinity versus brightness
  affinity_brightness = (rbind(affinity_tonic,affinity_octave) %>%
                           rotate(angle) * cos(angle)) %>% zapsmall
  # x [1,] is brightness polarity and y [2,] is affinity
  brightness_polarity = affinity_brightness[1,]
  affinity = affinity_brightness[2,]
  # we need more experimental data to determine the origin boundary for
  # brightness. our current approach uses the triangular nature of affinity
  # (1,3,6,10,15) to make a best guess that also aligns with the
  # experimental data with the Major 3rd and minor 6th
  # having the greatest positive and negative values of brightness.
  brightness_boundary = affinity %>% max %>% triangular_root
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  brightness = brightness_polarity / abs(affinity - brightness_boundary)
  # build the table
  tibble(
    semitone = intervals()$semitone,
    name = intervals()$name,
    brightness_polarity = brightness_polarity,
    affinity = affinity,
    brightness = brightness,
    tonic_gravity = sqrt(affinity^2 + brightness^2) * semitone,
    octave_gravity = sqrt(affinity^2 + brightness^2) * (12-semitone)
  )
}
