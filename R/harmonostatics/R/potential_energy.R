potential_energy <- function(affinity,brightness,home,semitone) {
  sqrt(affinity^2 + brightness^2) * abs(semitone-home)
}
