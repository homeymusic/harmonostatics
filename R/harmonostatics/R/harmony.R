harmony <- function() {
  affinity_tonic = affinity()$tonic
  affinity_octave = affinity()$octave
  # use the tritone to find rotation angle
  angle = atan2(affinity_octave[7],affinity_tonic[7])
  rotated_affinity = zapsmall(rbind(affinity_tonic,affinity_octave) %>% rotate(angle) * cos(angle))
  tibble(
    name = intervals()$name,
    polarity = rotated_affinity[1,],
    affinity = rotated_affinity[2,],
    brightness = polarity / affinity # psi = 2xy = const ±2
  )
}
