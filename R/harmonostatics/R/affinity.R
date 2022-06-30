affinity <- function(x) {
  checkmate::assert_integerish(x)
  ifelse (x %>% length < 2,
          calculate_affinity(x),
          combn(x,2,calculate_affinity) %>% mean
  )
}
calculate_affinity <- function(x) {
  # brightness polarity does NOT depend on level
  # affinity does NOT depend on the home note
  checkmate::qassert(x,c("X==1","X==2"))
  level_and_interval = level_and_interval_for(x)
  level = level_and_interval["level"]
  interval = level_and_interval["interval"]
  level_penalty = 2 * abs(level)
  if (abs(level) == 1) {level_penalty = 1}
  harmony.0.affinity()[interval+1] - level_penalty
}

#########
#
# level 0
#

affinity.0.tonic <- function() {
  tonic_disaffinity = disaffinity.0.tonic()
  tonic_disaffinity %>% max - tonic_disaffinity
}
affinity.0.octave <- function() {
  octave_disaffinity = disaffinity.0.octave()
  octave_disaffinity %>% max - octave_disaffinity
}
harmony.0.affinity <- function() {
  harmony.0.rotated_octave_affinity_tonic_affinity()[2,]
}
