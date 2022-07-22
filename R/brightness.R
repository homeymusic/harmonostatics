##############
#
# all levels
#

brightness.uncached <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  if (x %>% length == 1) {
    position_and_level = position_and_level_from_integer(x)
    brightness_all_levels(position_and_level[1],position_and_level[2])
  } else if (length(x)>1) {
    ifelse ((home==0),(x = x - min(abs(x))),(x = x + 12 - max(x)))
    sapply(x,function(x){
      position_and_level = position_and_level_from_integer(x)
      brightness_all_levels(position_and_level[1],position_and_level[2])
    },simplify=TRUE) %>% mean
  }
}
brightness <- memoise::memoise(brightness.uncached)

brightness_all_levels.uncached <- function(position,level=0) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  brightness_polarity = harmony_brightness_polarity()[position+1]
  brightness_from_affinity(brightness_polarity,affinity_all_levels(position,level))
}
brightness_all_levels <- memoise::memoise(brightness_all_levels.uncached)

# could have built this in a more fundamental and efficient way for level 0 but
# for all levels this is accurate and relies on the well-tested brightness code
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
brightness_boundary.uncached <- function() {
  # we need more major-minor experimental data to determine the boundary
  # for brightness. our current approach is to take the mean of the
  # minor 3rd, major 3rd, minor 6th and major 6th
  c(3,4,8,9) %>% sapply(affinity) %>% mean
}
brightness_boundary <- memoise::memoise(brightness_boundary.uncached)

brightness_from_affinity.uncached <- function(polarity,affinity) {
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  # x = 1 / y  ->  brightness = 1 / affinity
  centered_affinity = abs(affinity - brightness_boundary())
  ifelse(centered_affinity==0,
         0,
         polarity / centered_affinity
  )
}
brightness_from_affinity <- memoise::memoise(brightness_from_affinity.uncached)
