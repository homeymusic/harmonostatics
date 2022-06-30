brightness <- function(x,home) {
  checkmate::assert_choice(home,c(0,12))
  checkmate::assert_integerish(x)
  sapply(x,calculate_brightness,home) %>% mean
}

calculate_brightness <- function(x,home) {
  checkmate::qassert(x,"X1")
  checkmate::assert_choice(home,c(0,12))

  orig_affinity = affinity(x)

  ifelse(home==0,
         (x = x - min(x)),
         (x = x + 12 - max(x))
  )

  interval = level_and_interval_for(x)["interval"]
  brightness_polarity = harmony.0.brightness_polarity()[interval+1]
  brightness_for(brightness_polarity,orig_affinity)
}

brightness_for <- function(polarity,affinity) {
  # we use the stream function solution to the Laplace equation 2xy=const
  # with const = -2 and +2 for the relationship between brightness & affinity
  # x = 1 / y  ->  brightness = 1 / affinity
  centered_affinity = abs(affinity - harmony.0.brightness_boundary())
  ifelse(centered_affinity==0,
         0,
         polarity / centered_affinity
  )
}

#########
#
# level 0
#

harmony.0.brightness_polarity <- function() {
  harmony.0.rotated_octave_affinity_tonic_affinity()[1,]
}
harmony.0.brightness_boundary <- function() {
  # we need more major-minor experimental data to determine the boundary
  # for brightness. our current approach:
  harmony.0.brightness_boundary_triangular_root()
}
harmony.0.brightness_boundary_triangular_root <- function() {
  # use the triangular nature of affinity
  # (1,3,6,10,15) to make a best guess that also aligns with the
  # experimental data with the Major 3rd and minor 6th
  # having the greatest positive and negative values of brightness.
  harmony.0.affinity() %>% max %>% triangular_root
}
harmony.0.brightness <- function() {
  brightness_for(harmony.0.brightness_polarity(), harmony.0.affinity())
}

