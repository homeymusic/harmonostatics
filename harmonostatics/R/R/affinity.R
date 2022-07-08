# TODO: create an affinity_for(interval, level) function for handling multiple octaves

affinity.uncached <- function(x,home=0) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  length = x %>% length
  if (length == 1) {
    position_and_level = position_and_level_from_integer(x)
    affinity.0(position_and_level[1],position_and_level[2])
  } else {
    combn(x,2,calculate_affinity,simplify=TRUE,home) %>% mean
  }
}
affinity <- memoise::memoise(affinity.uncached)

calculate_affinity.uncached <- function(x,home) {
  checkmate::qassert(x,c("X==1","X==2"))
  checkmate::assert_choice(home,c(0,12))

  level_and_interval = level_and_interval_for(x,home)
  level = level_and_interval["level"]
  interval = level_and_interval["interval"]

  affinity.0(interval,level)
}
calculate_affinity <- memoise::memoise(calculate_affinity.uncached)

#########
#
# level 0
#

affinity.0 <- function(position,level=0) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")

  level_penalty = 0
  if (abs(level) == 1) {
    level_penalty = 1
  } else if (abs(level) > 1) {
    level_penalty = (2 * abs(level)) - 1
  }
  harmony.0.affinity()[position+1] - level_penalty
}

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
