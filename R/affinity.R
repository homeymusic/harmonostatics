##############
#
# all levels
#

affinity.uncached <- function(x) {
  checkmate::assert_integerish(x)
  if (x %>% length == 1) {
    position_and_level = position_and_level_from_integer(x)
    affinity_all_levels(position_and_level[1],position_and_level[2])
  } else {
    utils::combn(x,2,function(x){
      position_and_level = position_and_level_from_integer(abs(x[1] - x[2]))
      affinity_all_levels(position_and_level[1],position_and_level[2])
    },simplify=TRUE) %>% mean
  }
}
affinity <- memoise::memoise(affinity.uncached)

affinity_all_levels.uncached <- function(position,level=0) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")

  level_penalty = 0
  if (abs(level) == 1) {
    level_penalty = 1
  } else if (abs(level) > 1) {
    level_penalty = (2 * abs(level)) - 1
  }
  harmony_affinity()[position+1] - level_penalty
}
affinity_all_levels <- memoise::memoise(affinity_all_levels.uncached)

#########
#
# level 0
#

affinity_tonic_octave <- function() {
  rbind(affinity_tonic(),affinity_octave())
}
affinity_tonic <- function() {
  tonic_disaffinity = disaffinity_tonic()
  tonic_disaffinity %>% max - tonic_disaffinity
}
affinity_octave <- function() {
  octave_disaffinity = disaffinity_octave()
  octave_disaffinity %>% max - octave_disaffinity
}
