brightness.uncached <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  if (length(x)>1) {
    ifelse ((home==0),(x = x - min(abs(x))),(x = x + 12 - max(x)))
  }
  x = sapply(x,level_and_interval_for,0)[2,]
  # TODO: use brightness.0 instead of calculate_brightness.0
  sapply(x,calculate_brightness.0,home) %>% mean
}
brightness <- memoise::memoise(brightness.uncached)

calculate_brightness.0 <- function(x,home) {
  checkmate::qassert(x,"X1[0,12]")
  checkmate::assert_choice(home,c(0,12))
  brightness_from_affinity(harmony.0.brightness_polarity()[x+1],affinity(x))
}

#########
#
# level 0
#
brightness.0 <- function(position,level=0) {
  checkmate::assert_choice(position,0:12)
  checkmate::qassert(level,"X1")
  brightness_polarity = harmony.0.brightness_polarity()[position+1]
  brightness_from_affinity(brightness_polarity,affinity.0(position,level))
}
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
