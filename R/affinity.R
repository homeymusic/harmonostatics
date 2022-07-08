affinity.uncached <- function(x) {
  checkmate::assert_integerish(x)
  if (x %>% length == 1) {
    position_and_level = position_and_level_from_integer(x)
    affinity.0(position_and_level[1],position_and_level[2])
  } else {
    utils::combn(x,2,function(x){
      position_and_level = position_and_level_from_integer(abs(x[1] - x[2]))
      affinity.0(position_and_level[1],position_and_level[2])
    },simplify=TRUE) %>% mean
  }
}
affinity <- memoise::memoise(affinity.uncached)

#########
#
# level 0
#

affinity.0.uncached <- function(position,level=0) {
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
affinity.0 <- memoise::memoise(affinity.0.uncached)

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
