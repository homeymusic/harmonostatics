brightness <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  calculate_brightness(x,home)
}

calculate_brightness <- function(x,home) {
  polarity = calculate_brightness_polarity(x,home)
  brightness_for(polarity,affinity(x))
}

calculate_brightness_polarity <- function(x,home) {
  checkmate::assert_integerish(x)
  checkmate::assert_choice(home,c(0,12))
  if (length(x)==1) {
    min_max_test = c(home,x)
  } else {
    min_max_test = x
  }
  ifelse(home==0,
         (x = x - min(abs(min_max_test))),
         (x = x + 12 - max(abs(min_max_test)))
  )
  intervals = sapply(x,level_and_interval_for)[2,]

  brightness.0 = sapply(intervals,calculate_brightness.0) %>% mean
  if (brightness.0 < 0) {
    -1
  } else if (brightness.0 == 0) {
    0
  } else if (brightness.0 > 0) {
    1
  }
}

calculate_brightness.0 <- function(x) {
  checkmate::qassert(x,"X1")

  interval = level_and_interval_for(x)["interval"]

  brightness_polarity = harmony.0.brightness_polarity()[interval+1]
  brightness_for(brightness_polarity,affinity(x))
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
  harmony.0.brightness_boundary_3rds_and_6ths()
}
harmony.0.brightness_boundary_3rds_and_6ths <- function() {
  c(3,4,8,9) %>% sapply(affinity) %>% mean
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

