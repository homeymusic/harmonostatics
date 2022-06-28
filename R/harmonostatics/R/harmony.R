harmony <- function() {
  angle = pi / 4
  rotated_affinity = zapsmall(rbind(affinity()$tonic,affinity()$octave) %>% rotate(angle) * cos(angle))
  tibble(
    brightness = rotated_affinity[1,],
    affinity = rotated_affinity[2,]
  )
}
