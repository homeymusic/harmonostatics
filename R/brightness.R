brightness.uncached <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  if (x %>% length == 1) {
    position_and_level = position_and_level_from_integer(x)
    brightness.0(position_and_level[1],position_and_level[2])
  } else if (length(x)>1) {
    ifelse ((home==0),(x = x - min(abs(x))),(x = x + 12 - max(x)))
    sapply(x,function(x){
      position_and_level = position_and_level_from_integer(x)
      brightness.0(position_and_level[1],position_and_level[2])
    },simplify=TRUE) %>% mean
  }
}
brightness <- memoise::memoise(brightness.uncached)

brightness_polarity.uncached <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  brightness = brightness(x,home)
  if (brightness<0) {
    -1
  } else if (brightness==0) {
    0
  } else if (brightness > 0) {
    1
  }
}
brightness_polarity <- memoise::memoise(brightness_polarity.uncached)
#########
#
# level 0
#
brightness.0.uncached <- function(position,level=0) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  brightness_polarity = harmony.0.brightness_polarity()[position+1]
  brightness_from_affinity(brightness_polarity,affinity.0(position,level))
}
brightness.0 <- memoise::memoise(brightness.0.uncached)

harmony.0.brightness_polarity <- function() {
  harmony.0.rotated_octave_affinity_tonic_affinity()[1,]
}
harmony.0.brightness_boundary <- function() {
  # we need more major-minor experimental data to determine the boundary
  # for brightness. our current approach:
  harmony.0.brightness_boundary_3rds_and_6ths()
}
harmony.0.brightness <- function() {
  brightness_from_affinity(harmony.0.brightness_polarity(), harmony.0.affinity())
}
harmony.0.brightness_boundary_3rds_and_6ths <- function() {
  c(3,4,8,9) %>% sapply(affinity) %>% mean
}
brightness_from_affinity.uncached <- function(polarity,affinity) {
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  # x = 1 / y  ->  brightness = 1 / affinity
  centered_affinity = abs(affinity - harmony.0.brightness_boundary())
  ifelse(centered_affinity==0,
         0,
         polarity / centered_affinity
  )
}
brightness_from_affinity <- memoise::memoise(brightness_from_affinity.uncached)
