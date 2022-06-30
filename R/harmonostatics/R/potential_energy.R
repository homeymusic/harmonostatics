potential_energy <- function(affinity,brightness,home,semitone) {
  semitone_difference = abs(semitone-home)
  affinity_difference = abs(affinity - affinity(home))
  brightness_difference = abs(brightness - brightness(home,home))
  sqrt(affinity_difference^2 + brightness_difference^2) * semitone_difference
}
